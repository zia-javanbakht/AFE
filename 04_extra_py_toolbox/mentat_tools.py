# =======
# Modules
# =======

from py_post import *
from py_mentat import *
import numpy as np
import sys
import os
import xlwt
import time
import pickle


# ================
# Global variables
# ================

# This is a general variable indicating the selection tolerance.
# It is used for sweeping and selection of entities like nodes and points.
TOL = 0.000001

# =========
# Functions
# =========


# **************************
# General purpose functions
# **************************


#
# Selects nodes.
# If only two coordinates are passed, it is assumed that nodes are on the z=0 plane.
# Now a return value should be used to indicate a successful node selection.
#


def copy_file(src, dest):
	"""Copies a file from source to destination path."""
	try:
		print('\tCopying', src, 'to', dest + '...', end='')
		os.system('copy %s %s' % (src, dest))
	except Exception as e:
		print('Error! ', str(e), e.args)
	else:
		print('Done!')
	return


def move_file(src, dest):
	"""Moves a file from source to destination path."""
	try:
		print('\tMoving', src, 'to', dest + '...', end='')
		os.system('move %s %s' % (src, dest))
	except Exception as e:
		print('Error! ', str(e), e.args)
	else:
		print('Done!')
	return


def pretty(m):
	"""Pretty prints a matrix in formatted table style."""
	matrix = m.tolist()
	s = [[str(e) for e in row] for row in matrix]
	lens = [max(map(len, col)) for col in zip(*s)]
	fmt = '\t'.join('{{:{}}}'.format(x) for x in lens)
	table = [fmt.format(*row) for row in s]
	print('\n'.join(table))
	return


def write_excel(sheet, row, col, data):
	"""Writes data list to Excel sheet starting from specified row and column. Returns the current row and current column number."""
	for data_item in data:
		sheet.write(row, col, data_item)
		row += 1
	col += 1
	return row, col





def save_RVE (RVE_object, RVE_fn):
	"""Saves RVE object to file using pickle serialization."""
	print ('> Saving ', RVE_fn,'...',end='')
	start_time = time.time()
	
	with open(RVE_fn, 'wb') as f:
		pickle.dump (RVE_object, f)
	print ('done in %s seconds! Enjoy your pickle!' % (time.time() - start_time))
	return

def load_RVE (RVE_fn):
	"""Loads RVE object from pickle file."""
	print ('> load_RVE()...\n\tloading', RVE_fn,'...',end='')
	try:
		with open(RVE_fn, 'rb') as f:
			newRVE = pickle.load(f)
	except Exception as e:
		print ('error! ', str(e), e.args)
	else:
		print ('done! Enjoy your pickle!')
	return newRVE


def read_post_file (rfn, postLabel, nodIDLst):
	"""
Gets a nodal scalar output from a result file.
	
	Args:
		rfn: result file name
		postLabel: label for the nodal result  
		nodIDLst: a list of external ID for the node
	
	Returns:
		an array of the requested nodal values
	"""

	# open the post file and try to move to first increment
	try:
		p = post_open(rfn)
	except:
		print ('> read_post_file():\tError reading the post file ', rfn)
		sys.exit()

#-------------------------------------------------------------------------------------------------------------------------
# pyPost works with nodeIndex (or internal node number) make sure a renumbering is performed in the model before analysis.
#   Actually the number of returned increments will be 2 more than what is in the post file since the increment zero of the
# post file is increment 1 while using p.moveto(1), that is:
# moveto(1)   for increment 0
# moveto(2)   for increment 1
# ...
# moveto(n)   for increment n-1
# moveto(n+1) for increment n
#
# The additional one is because in a loop like range(1,nInc) the loop runs until nInc-1, so the user can easily write:
#   for i in range(1,ninc):
#      stuff to be done for each increment...
#-------------------------------------------------------------------------------------------------------------------------


	nInc = p.increments()
	#    print "Number of increments: ", nInc - 2


	# Number of scalars will not be available in increment zero, and thus the current increment should be at least equal to one.
	#    nNodScalars = p.node_scalars()
	#    print "Number of total scalars: ", nNodScalars

	scalarLst = []

	# Note that based on the number of increment, the number of manu values may change, such as the following items:
	#   total number of elements/nodes
	#   index number of elements/nodes
	#   available scalar element/nodal values
	try:
		p.moveto(1)
	except:
		print ("> Error opening post file:", rfn)
		return

	# Since it is not known that what is requested is a nodal or elemental result, both of them should be checked. Here, only the nodal scalars are considered.
	# Checking among scalars
	nNodScalars = p.node_scalars()
	found = False
	i = 0

	# compares the uppercase POST labels to avoid case problems
	while (found == False and i < nNodScalars):
		if p.node_scalar_label(i).upper() == postLabel.upper():
			found = True
			scalarIndex = i
		i = i + 1

	if (found == True):
		print ("The index for ", postLabel, " is ", scalarIndex)
	else:
		print ("No index was found for ", postLabel)

	# List all the scalar values (only for debugging)
	#    for i in range (0, nNodScalars):
	#        print i,": ", p.node_scalar_label(i)

	#   get the results for increment 1 onwards
	for i in range (1, nInc):
		p.moveto(i)

	cur_node_scalar = []

	# change the nodID to nodIndex since everything in PyPost works with the index
	for iNod in nodIDLst:
		nodIndex = p.node_sequence(iNod)
		#        print "Node ID ", nodID, " is indexed as ", nodIndex

		cur_node_scalar.append (p.node_scalar(nodIndex, scalarIndex))
	
	scalarLst.append (cur_node_scalar)
	# print "Obtained value in increment ", p.increment , " is ", p.node_scalar(nodIndex, scalarIndex)


	# definitely the file should be closed after using
	p.close( )

	return scalarLst



def create_folder(dirName):
	"""Creates a directory if it doesn't already exist."""
	# Create target Directory if don't exist
	if not os.path.exists(dirName):
		os.mkdir(dirName)
		print("> Directory" , dirName ,  "created!")
	else:    
		print("> Directory" , dirName ,  "already exists!")
	return


def GetTOL():
	"""Returns the global tolerance value."""
	return TOL

def SetTOL(tol):
	"""Sets the global tolerance value."""
	global TOL
	TOL = tol
	return

def SelectNod (nodCoord):
	"""Selects nodes at specified coordinates within tolerance. Returns number of selected nodes."""
	
	nSelectBefore = py_ms_int ('select_node_count',0)
	# print 'before', nSelectBefore
	# print 'tol', TOL
	
	py_send ('*select_method_box')
	py_send ('*select_mode_and')
	py_send ('*select_nodes')
	py_send ('%f %f' %(nodCoord[0]- TOL, nodCoord[0]+TOL))
	py_send ('%f %f' %(nodCoord[1]- TOL, nodCoord[1]+TOL))
	if len(nodCoord) == 3:
		py_send ('%f %f' %(nodCoord[2]-TOL, nodCoord[2]+TOL))
	else:
		py_send ('%f %f' %(-TOL, +TOL))
	py_send ('')
	
	nSelectAfter = py_ms_int ('select_node_count',0)
	# print 'after', nSelectAfter
	return nSelectAfter - nSelectBefore
	

def SelectNodBox (aBox):
		"""Selects nodes within a bounding box (2D or 3D). Returns number of selected nodes."""
		nSelectBefore = py_ms_int ('select_element_count',0)
		py_send ('*select_method_box')
		py_send ('*select_mode_and')
		py_send ('*select_nodes')
		py_send ('%f %f' %(aBox[0][0], aBox[1][0]))
		py_send ('%f %f' %(aBox[0][1], aBox[1][1]))
	
		if len(aBox[0]) == 3:
			py_send ('%f %f' %(aBox[0][2], aBox[1][2]))
		else:
			py_send ('%f %f' %(-TOL, +TOL))  
		py_send ('')
	
		nSelectAfter = py_ms_int ('select_nod_count',0)
	
		return nSelectAfter - nSelectBefore













# Creates points if 3 coordinates are provided.
def AddPnt (pntCoord):
	"""Creates a point at specified coordinates (2D or 3D)."""
	py_send ('*add_points')
	
	if len(pntCoord) == 3:
		py_send ('%f %f %f' %(pntCoord[0], pntCoord[1], pntCoord[2]))
	else:
		py_send ('%f %f %f' %(pntCoord[0], pntCoord[1], 0.0))
	py_send ('')

	return


# Creates points if 3 coordinates are provided.
def AddNod (nodCoord):
	"""Creates a node at specified coordinates (2D or 3D)."""
	py_send ('*add_nodes')
	
	if len(nodCoord) == 3:
		py_send ('%f %f %f' %(nodCoord[0], nodCoord[1], nodCoord[2]))
	else:
		py_send ('%f %f %f' %(nodCoord[0], nodCoord[1], 0.0))
	py_send ('')

	return


# Sends a list of nodes to create an element
def AddElm (nodLst):
	"""Creates an element from a list of node IDs."""
	py_send ('*add_elements')
	for i in range(len(nodLst)):
		py_send ('%d'  % nodLst[i])
	py_send ('')
	return




def SelectElm (aBox):
	"""Selects elements within a bounding box. Returns number of selected elements."""

	nSelectBefore = py_ms_int ('select_element_count',0)
	py_send ('*select_method_box')
	py_send ('*select_mode_and')
	py_send ('*select_elements')
	py_send ('%f %f' %(aBox[0][0], aBox[1][0]))
	py_send ('%f %f' %(aBox[0][1], aBox[1][1]))
	
	if len(aBox[0]) == 3:
		py_send ('%f %f' %(aBox[0][2], aBox[2][1]))
	else:
		py_send ('%f %f' %(-TOL, +TOL))  
	py_send ('')
	
	nSelectAfter = py_ms_int ('select_element_count',0)
	
	return nSelectAfter - nSelectBefore


def GetNodID (coord):
	"""Returns the node ID at specified coordinates, or -1 if not found or multiple nodes selected."""
	py_send ('*select_clear')
	if SelectNod (coord) == 1:
		py_send ('*store_nodes')
		py_send ('_temp')
		py_send ('all_selected')
		py_send ('')
	
		setID = GetSetID('_temp')
		# print 'setID', setID
		nodID = py_get_int('set_entry(%d, %d)' %(setID,1))
		# print 'nodID', nodID
		py_send ('*remove_sets _temp')
	else:
		# Indicating an error: either no coord was selected or more than one node was selected.
		nodID = -1
		print ('GetNodID Error')
	return nodID
	

def GetElmID (aBox):
	"""Returns the element ID within a bounding box, or -1 if not found or multiple elements selected."""
	py_send ('*select_clear')
	if SelectElm (aBox) == 1:
		py_send ('*store_elements')
		py_send ('_temp')
		py_send ('all_selected')
		py_send ('')
	
		setID = GetSetID('_temp')
		elmID = py_get_int('set_entry(%d, %d)' %(setID,1))

		py_send ('*remove_sets _temp')
	else:
		# Indicating an error: either no coord was selected or more than one node was selected.
		elmID = -1
		print ('GetElmID Error')
	return elmID

	
	
def GetSetID (setName):
	"""Returns the ID of a named set, or -1 if not found."""
	nSets = py_get_int ('nsets()')
	
	notFound = True
	i = 1
	while (i < nSets + 1) and (notFound):
		setID   = py_get_int('set_id(%d)' %i)
		curSetName = py_get_string ('set_name(%d)' %setID)

		if setName.upper() == curSetName.upper():
			notFound = False
		i += 1

	if notFound == True:
		# Indicating an error since nothing was found!
		setID = -1
		print ('GetSetID Error')
	return setID



# In py_post, there is another mechanism to deal with sets.
# All the functions which are to be used in the post processing will have a 'post' suffix.



	
# Returns the items of a set
def get_set_item (set_name):
	"""Returns a list of items (IDs) contained in the specified set."""
	set_ID    = GetSetID (set_name)
	
	if set_ID != -1:
		n_set     = py_get_int('nset_entries (%d)'%set_ID )
		print ('nset=', n_set)
		item_lst  = []
		
		for i in range(1,n_set+1):
			cur_item = py_get_int('set_entry (%d, %d)' %(set_ID, i))
			item_lst.append(cur_item)
	else:
		print ("Set not found!")
		item_lst = -1

	return item_lst
	



# # Module testing
# def main():
#   print get_set_item ('contact')
#   return
#   
# # call to the main function
# if __name__ == '__main__':
#   main()  






# ---------------------------
# Make element sets invisible
# ---------------------------
def MakeNodSetInvis (setName):
	"""Makes a node set invisible."""
	py_send ('*select_method_single')
	py_send ('*select_nodes')
	py_send ('*select_sets %s' % setName) 
	py_send ('*invisible_selected')
	return

# -------------------------
# Make element sets visible
# -------------------------
def MakeNodSetVis (setName):
	"""Makes a node set visible."""
	py_send ('*select_method_single')
	py_send ('*select_nodes')
	py_send ('*select_sets %s' % setName) 
	py_send ('*visible_selected')
	return

# ------------------------
# Make node sets invisible
# ------------------------
def MakeElmSetInvis (setName):
	"""Makes an element set invisible."""
	py_send ('*select_method_single')
	py_send ('*select_elements')
	py_send ('*select_sets %s' % setName)
	py_send ('')  
	py_send ('*invisible_selected')
	return

# ----------------------
# Make node sets visible
# ----------------------
def MakeElmSetVis (setName):
	"""Makes an element set visible."""
	py_send ('*select_method_single')
	py_send ('*select_elements')
	py_send ('*select_sets %s' % setName) 
	py_send ('')
	py_send ('*visible_selected')
	return


def CreateElmSet (setName):
	"""Creates an element set containing all existing elements in the model."""
	py_send ('*store_elements %s' % setName)
	py_send ('all_existing')
	return


def CreateVisElmSet (setName):
	"""Creates an element set containing all visible elements in the model."""  
	py_send ('*store_elements %s' % setName)
	py_send ('all_visible')
	return

# --------------------------------
# Create node set from all visible
# --------------------------------
def CreateNodSet (setName):
	"""Creates a node set containing all existing nodes."""
	py_send ('*store_nodes %s' % setName)
	py_send ('all_existing')
	return



def put_select_node_set (setName):
	"""Stores all selected nodes into a named set."""
	py_send ('*store_nodes %s' % setName)
	py_send ('all_selected')
	return


# -----------------------------
# Run a job from command prompt
# -----------------------------

#****************************************************************
# submitting by Mentat is not good because the commands run
# one after another and the following commands will not wait
# for the current job to finish:
#
#    py_send ("*submit_job 1 *monitor_job")
#    print "Submitting the job..."
# non-parallel run
#    os.system ("run_marc -j lat_cylinder01_job1 -prg u -bg y")
# parallel run
#****************************************************************
def run_job_multi(*args, **kw_args):
	"""Runs a MARC job with multi-threading support (48 threads). Supports optional Fortran subroutine or executable."""
	num_kw_args = len(kw_args.items())
	if (num_kw_args > 0):
		if kw_args['subroutine'] == 'fortran':
			job_name, fortran_sub = args
			os.system ("run_marc -j %s -bg n -nthread_elm 48 -nthread_solver 48 -nsolver 48 -u %s" % (job_name,fortran_sub))
		elif kw_args['subroutine'] == 'exe':
			job_name, fortran_sub = args
			os.system ("run_marc -j %s -bg n -nthread_elm 48 -nthread_solver 48  -nsolver 48 -prg %s" % (job_name,fortran_sub))
	else:
		job_name = args[0]
		os.system ("run_marc -j %s -bg n -nthread_elm 48 -nthread_solver 48 -nsolver 48" % job_name)
	return

# single core
def run_job(*args, **kw_args):
	"""Runs a MARC job on single core. Supports optional Fortran subroutine or executable."""
	num_kw_args = len(kw_args.items())
	if (num_kw_args > 0):
		if kw_args['subroutine'] == 'fortran':
			job_name, fortran_sub = args
			os.system ("run_marc -j %s -bg n -u %s" % (job_name,fortran_sub))
		elif kw_args['subroutine'] == 'exe':
			job_name, fortran_sub = args
			os.system ("run_marc -j %s -bg n -prg %s" % (job_name,fortran_sub))
	else:
		job_name = args[0]
		os.system ("run_marc -j %s -bg n " % job_name)
	return



# ---------------------
# Writes the input file
# ---------------------
def write_input_file():
	"""Writes the input file for the current job."""
	py_send ("*job_write_input yes")
	return

def rename_file (oldName, newName):
	"""Renames a file from oldName to newName."""
	try:
		os.rename (oldName, newName)
	except OSError as err:
		print ("Error while renaming '%s' to '%s'!\n %s" % (oldName, newName,err))
	return


def backup_file (filename):
	"""Backs up a file by renaming it with a sequential number suffix to prevent overwriting the original file."""
	# if an export file exists, rename it.
	file_exists = os.path.isfile (filename)
	i = 1

	# filename will hold the most recent data
	if file_exists:
		while file_exists:
			file_name, file_extension = os.path.splitext(filename)
			new_filename = file_name + '%04d' %(i) + file_extension
			file_exists = os.path.isfile (new_filename)
			i+=1
		os.rename(filename, new_filename)
	return
		





def GetNodElmNum ():
	"""Returns the starting node and element numbers after renumbering. The numbering starts after their total numbers provided that a renumber command is executed."""
	py_send ('*sweep_all')
	py_send ('*remove_unused_nodes')
	py_send ('*renumber_nodes')
	py_send ('*renumber_elements')
	
	nN = py_get_int ('nnodes()')
	nE = py_get_int ('nelements()')
	return nN+1, nE+1

#----------------------
# Get members of a set
#----------------------
def GetSetMembers (setName):
	"""Returns a list of member IDs for the specified set."""
# extract the number of sets
	nSet = py_get_int("nsets()")

# report
	#print ("  SETS Found:")
	#print ("  -----------")
	#for i in range(1,nSet+1):
	#	sid       = py_get_int("set_id(%d)" % i)
	#	setname   = py_get_string("set_name(%d)" % sid)
	#	settype   = py_get_string("set_type(%d)" % sid)
	#	setsize   = py_get_int("nset_entries(%d)" % sid)
	#	outstring = "      %20s:  %s set with %d entries" % (setname, settype, setsize)
	#	print (outstring)

# find the ID of the set
	found = False
	iSet  = 1
	while (not found) & (iSet <= nSet):
		iSetID   = py_get_int("set_id(%d)" % iSet)
		iSetName = py_get_string("set_name(%d)" % iSetID)

		if (iSetName == setName):
			found = True
			setID = iSetID
		else:
			iSet += 1

	if not found:
		print ("\nERROR: Either no element set or the wrong setname has been specified (",setName,")\n")
	return 1

#-------------------------------
# extract the members of the set

	memberLst = []
# number of members in set
	nMember = py_get_int("nset_entries(%d)" % setID)
	for i in range (1, nMember+1):
		iMember = py_get_int("set_entry(%d,%d)" % (setID, i))
		memberLst.append (iMember)

	return memberLst



def GUI_update (key):
	"""Toggles GUI updates on/off. True enables updates, False disables for performance."""

	if (key):
		## turn off the GUI updates 
		## set the echo of the software off
		py_send ('*draw_automatic')
		py_send ('*model_navigator_update on')
		py_send ('*set_update on')
		py_send ('*set_undo on')
		# turns off 3D distance consideration during drawing
		py_send ('*set_zbuffered on')
		py_send ('*py_echo on')
		py_echo (1)
	else:
		## turn off the GUI updates 
		## set the echo of the software off
		py_send ('*draw_manual')
		py_send ('*model_navigator_update off')
		py_send ('*set_update off')
		py_send ('*set_undo off')
		# turns off 3D distance consideration during drawing
		py_send ('*set_zbuffered off')
		py_send ('*py_echo off')
		py_echo (0)

	return


def set_mesh_density (gamma):
	"""Sets the mesh density parameter gamma."""
	tCmd = "*define gamma %f" % (gamma)
	py_send (tCmd)
	return

def change_mod (elMod):
	"""Changes the Young's modulus of material1."""
	py_send ("*edit_mater material1")
	tCmd = "*mater_param structural:youngs_modulus %f" % (elMod)
	py_send (tCmd)
	return

def change_poi (pRatio):
	"""Changes the Poisson's ratio of material1."""
	py_send ("*edit_mater material1")
	py_send ("*mater_param structural:poissons_ratio %f" % pRatio)
	return

def GetErr():
	"""Reads error value from elMod.txt file."""
	errFile = open ("elMod.txt", "r")
	return float(errFile.read(20))

def send_msg (el, err):
	"""Sends a message with Young's modulus and error values to the GUI."""
	tCmd = "py_prompt 'E= %f Error=%d'" % (el, err)
	py_send(tCmd)
	return

def UpdateElMod(p0,p1,ep0,ep1):
	"""Updates elastic modulus using secant method of Newton-Raphson."""
# secant method of NR is used
	return p1 - ep1 /((ep1-ep0)/(p1-p0))

def calc_rel_err (p1,p0):
	"""Calculates relative error between two values."""
	temp = abs((p1-p0)/p1)
	return temp

def rename_file (oldName,newName):
	"""Renames a file from oldName to newName (duplicate function)."""
	try:
		os.rename (oldName, newName)
	except OSError as err:
		print ("Error while renaming '%s' to '%s'!\n %s" % (oldName, newName,err))
	return

def set_vol_frac (volFrac):
	"""Writes volume fraction value to volfrac.dat file."""
	f = open ('volfrac.dat', 'w')
	f.write('%5.3f\n' % (volFrac) )
	f.close()
	return

def read_file(fn):
	"""Reads data from a formatted file and extracts three float values from columns 2-4."""
	data = []
	with open (fn) as f:
		# just read the header lines
		header = [next(f) for i in range(1,6)]
		# read the data
		for i in range (0,1):
			dataline = next(f)
			columns = dataline.split()
			print (columns[2])
			print (columns[3])
			print (columns[4])
			data.append(float(columns[2]))
			data.append(float(columns[3]))
			data.append(float(columns[4]))
	f.close()
	return data


# main() function
def main():
	"""Main function that demonstrates GetNodElmNum functionality."""
	# clears the output screen
	a, b = GetNodElmNum()
	print (a,b)    
	return

# Redirect MARC/MENTAT to main()
if __name__ == '__main__':
	main()