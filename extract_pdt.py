#Gaussian data extraction

import re
import csv

#opens the file as both a string and list

print("Extract product data (6-membered ring)")
file_input = input("What is the file name?: ")
filename = file_input + '.out'
file_output = 'ML_pdt.csv'
	
try:
	with open(filename) as file_object:
		lines = file_object.readlines()
		contents = str(lines)
		words = contents.split()
except IOError:
	msg = "Sorry, the file " + filename + " does not exist."
	print(msg)
	quit()

def in_between(text_list, start_keyword, end_keyword): 
	"""Takes a list of text and returns the list containing entries
	between start_keyword and end_keyword (inclusive).
	"""
	output = []
	parsing = False
	for line in text_list:
		if start_keyword in line: 
			parsing = True 
		if parsing:
			output.append(line) 
		if end_keyword in line: 
			parsing = False
	return output

#extracts relevant regions of the files

geometry_start = 'Optimized Parameters'
geometry_end = '*******         Alpha spin orbitals'
geometry = in_between(lines, geometry_start, geometry_end)

NBO_start = 'N A T U R A L   A T O M I C   O R B I T A L'
NBO_end = '*******         Alpha spin orbitals'
NBO = in_between(geometry, NBO_start, NBO_end)

#get total number of atoms

tot_atom_keyword = 'NAtoms=     '
tot_atoms = 101

for line in lines:
	if tot_atom_keyword in line:
		tot_atoms = line.split()
		tot_atoms = tot_atoms[1]

#useful functions

def get_bond_order(atom_A, atom_B, tot_atoms=tot_atoms):
	"""get bond order for any combination of atom_A and atom_B"""
	bond_order_start = 'Wiberg bond index matrix in the NAO basis:'
	bond_order_end = 'Wiberg bond index, Totals by atom:'
	bond_order_lines = in_between(NBO, bond_order_start, bond_order_end)
	bond_orders = ''
	for bond_order_line in bond_order_lines:
		if ' ' + str(atom_A) + '. ' in bond_order_line: 
			bond_orders += bond_order_line
	bond_orders = re.sub('[^.,0-9]', ' ', bond_orders)
	bond_orders = re.sub(' ' + str(atom_A) + '. ', '', bond_orders)
	bond_orders = bond_orders.split()
	bond_order = bond_orders[-(1 + int(tot_atoms) - int(atom_B))]
	return(bond_order)

def get_bond_length(atom_A, atom_B):
	"""get bond length for any combination of atom_A and atom_B"""
	bond_len_start = '! (Angstroms and Degrees)  !'
	bond_len_end = 'GradGradGradGradGradGradGradGradGradGrad'
	bond_len_lines = in_between(geometry, bond_len_start, bond_len_end)
	bond_length = ''
	for bond_len_line in bond_len_lines:
		if 'R(' + str(atom_A) + ',' + str(atom_B) + ')' in bond_len_line: 
			bond_length = bond_len_line
			bond_length = bond_length.split()
		elif 'R(' + str(atom_B) + ',' + str(atom_A) + ')' in bond_len_line: 
			bond_length = bond_len_line
			bond_length = bond_length.split()
	return(bond_length[3])

def get_bond_angle(atom_A, atom_B, atom_C):
	"""get bond angle for any combination of atoms A, B, and C"""
	bond_ang_start = '! (Angstroms and Degrees)  !'
	bond_ang_end = 'GradGradGradGradGradGradGradGradGradGrad'
	bond_ang_lines = in_between(geometry, bond_ang_start, bond_ang_end)
	bond_angle = ''
	for bond_ang_line in bond_ang_lines:
		if 'A(' + str(atom_A) + ',' + str(atom_B) + ',' + str(atom_C) + ')' in bond_ang_line: 
			bond_angle = bond_ang_line
			bond_angle = bond_angle.split()
		elif 'A(' + str(atom_C) + ',' + str(atom_B) + ',' + str(atom_A) + ')' in bond_ang_line: 
			bond_angle = bond_ang_line
			bond_angle = bond_angle.split()
	return(bond_angle[3])

def get_atom_BO(atom_A):
	"""get total bond order for any atom A"""
	BO_start = "Wiberg bond index, Totals by atom: "
	BO_end = "Atom-atom overlap-weighted NAO bond order:"
	atom_BO = []
	atom_lines = in_between(geometry, BO_start, BO_end)
	for atom_line in atom_lines:
		if str(atom) + ". " in atom_line:
			atom_BO += atom_line.split()
	return(atom_BO[2])

def get_electrons(atom_A, parameter):
	"""
	returns the electron data for a number of parameters,
	including charge, core electrons, valence electrons,
	Rydberg electrons, and total electrons
	"""
	electron_start = 'Summary of Natural Population Analysis:'
	electron_end = '====================================='
	electron = ''
	electron_lines = in_between(geometry, electron_start, electron_end)
	for electron_line in electron_lines:
		if " " + str(atom_A) + " " in electron_line:
			electron += electron_line
			electron = electron.split()
			electron = electron[(int(parameter))]
	return(electron)

def get_NMR(atom_A):
	"""returns the raw NMR shift"""
	NMR_start = 'SCF GIAO Magnetic shielding'
	NMR_end = 'PrsmSu:'
	NMR_shift= ''
	NMR_lines = in_between(lines, NMR_start, NMR_end)
	for NMR_line in NMR_lines:
		if " " + str(atom_A) + "  " in NMR_line:
			NMR_shift += NMR_line
			NMR_shift = NMR_shift.split()
	return(NMR_shift[4])


#set atom numbering for atoms 1 through 11

last_line = lines[-1]
if "Gaussian" in last_line:
	mapping_numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
	atom_numbers = []
	for mapping_number in mapping_numbers:
		ask_atom = input("What atom would you like to assign as atom " + str(mapping_number) + '?: ')
		atom_numbers.append(str(ask_atom))
	atom_1 = atom_numbers[0]
	atom_2 = atom_numbers[1]
	atom_3 = atom_numbers[2]
	atom_4 = atom_numbers[3]
	atom_5 = atom_numbers[4]
	atom_6 = atom_numbers[5]
	atom_7 = atom_numbers[6]
	atom_8 = atom_numbers[7]
	atom_9 = atom_numbers[8]
	atom_10 = atom_numbers[9]
	atom_11 = atom_numbers[10]
	atoms = [
		atom_1,
		atom_2,
		atom_3,
		atom_4,
		atom_5,
		atom_6,
		atom_7,
		atom_8,
		atom_9,
		atom_10,
		atom_11
		]
	with open(filename, 'a') as file_object:
		file_object.write(str(atoms))
else:	
	last_line = re.sub('[^0-9,]', '', last_line)
	atoms = last_line.split(',')
	atom_1 = atoms[0]
	atom_2 = atoms[1]
	atom_3 = atoms[2]
	atom_4 = atoms[3]
	atom_5 = atoms[4]
	atom_6 = atoms[5]
	atom_7 = atoms[6]
	atom_8 = atoms[7]
	atom_9 = atoms[8]
	atom_10 = atoms[9]
	atom_11 = atoms[10]

#
data = []
data.append(file_input)

#get molecular descriptors
#prints the dipole moment

print("\nEXTRACTING DESCRIPTORS")

dipole_startword = 'Dipole moment'
dipole_endword = 'X='

dipole_moment = in_between(geometry, dipole_startword, dipole_endword)
dipole_moment = str(dipole_moment[-1]).split()
#print("Dipole moment: " + dipole_moment[-1])
data.append(dipole_moment[-1])

#prints the dG energy

dG_keyword = 'Sum of electronic and thermal Free Energies='
dG_value = ''

for line in lines:
	if dG_keyword in line:
		dG_value += line
		dG_value = dG_value.split()
#print("dG value: " + dG_value[-1])
data.append(dG_value[-1])

#prints the HOMO and LUMO energies and energy gap

HOMO_keyword = "Alpha  occ. eigenvalues"
LUMO_keyword = "Alpha virt. eigenvalues"

HOMO_LUMO_energies = in_between(geometry, HOMO_keyword, LUMO_keyword)
HOMO_energy = HOMO_LUMO_energies[-2].split()
HOMO_energy = HOMO_energy[-1]
#print("HOMO energy: " + HOMO_energy)
data.append(HOMO_energy)

LUMO_energy = HOMO_LUMO_energies[-1].split()
LUMO_energy = LUMO_energy[4]
#print("LUMO energy: " + LUMO_energy)
data.append(LUMO_energy)

HOMO_LUMO_gap = float(HOMO_energy) - float(LUMO_energy)
#print("HOMO/LUMO energy gap: " + str(round(HOMO_LUMO_gap, 5)))
data.append(str(round(HOMO_LUMO_gap, 5)))

#prints electronegativity, hardness, and electrophilicity

electroneg = -0.5 * (float(LUMO_energy) + float(HOMO_energy))
#print("electronegativity: " + str(round(electroneg, 5)))
data.append(str(round(electroneg, 5)))

hardness = 0.5 * (float(LUMO_energy) - float(HOMO_energy))
#print("hardness: " + str(round(hardness, 5)))
data.append(str(round(hardness, 5)))

electrophil = (float(electroneg) ** 2) / (2 * float(hardness))
#print("electrophilicity: " + str(round(electrophil, 5)))
data.append(str(round(electrophil, 5)))

#
#
#
#

#get bond lengths

#print("\nBOND LENGTHS")
data.append(get_bond_length(atom_1, atom_2))
data.append(get_bond_length(atom_2, atom_3))
data.append(get_bond_length(atom_3, atom_4))
data.append(get_bond_length(atom_4, atom_5))
data.append(get_bond_length(atom_5, atom_6))
data.append(get_bond_length(atom_1, atom_7))
data.append(get_bond_length(atom_1, atom_8))
data.append(get_bond_length(atom_5, atom_9))
data.append(get_bond_length(atom_6, atom_10))
data.append(get_bond_length(atom_6, atom_11))
data.append(get_bond_length(atom_1, atom_6))


#get bond angles
#print("\nBOND ANGLES")
data.append(get_bond_angle(atom_1, atom_2, atom_3))
data.append(get_bond_angle(atom_2, atom_3, atom_4))
data.append(get_bond_angle(atom_3, atom_4, atom_5))
data.append(get_bond_angle(atom_4, atom_5, atom_6))
data.append(get_bond_angle(atom_2, atom_1, atom_7))
data.append(get_bond_angle(atom_7, atom_1, atom_8))
data.append(get_bond_angle(atom_4, atom_5, atom_9))
data.append(get_bond_angle(atom_5, atom_6, atom_10))
data.append(get_bond_angle(atom_5, atom_6, atom_11))
data.append(get_bond_angle(atom_2, atom_1, atom_6))
data.append(get_bond_angle(atom_2, atom_1, atom_8))
data.append(get_bond_angle(atom_1, atom_6, atom_5))
data.append(get_bond_angle(atom_5, atom_6, atom_10))
data.append(get_bond_angle(atom_10, atom_6, atom_11))


#get bond orders
#print("\nBOND ORDERS")
data.append(get_bond_order(atom_1, atom_2,)) 
data.append(get_bond_order(atom_2, atom_3,))
data.append(get_bond_order(atom_3, atom_4,))
data.append(get_bond_order(atom_4, atom_5,))
data.append(get_bond_order(atom_5, atom_6,))
data.append(get_bond_order(atom_1, atom_7,))
data.append(get_bond_order(atom_1, atom_8,))
data.append(get_bond_order(atom_5, atom_9,))
data.append(get_bond_order(atom_6, atom_10,))
data.append(get_bond_order(atom_6, atom_11,))
data.append(get_bond_order(atom_1, atom_6,))

#get bond order totals by atom

#print('')
for atom in atoms:
	data.append(get_atom_BO(str(atom)))

#get electron data

charge = 2
core = 3
valence = 4
Rydberg = 5
total = 6

#print("\nELECTRON DATA")
for atom in atoms:
	data.append(get_electrons(atom, charge))
	
#print("")
for atom in atoms:
	data.append(get_electrons(atom, core))
	
#print("")
for atom in atoms:
	data.append(get_electrons(atom, valence))
	
#print("")
for atom in atoms:
	data.append(get_electrons(atom, Rydberg))
	
#print("")
for atom in atoms:
	data.append(get_electrons(atom, total))

#get NMR data

#print("\nNMR data")
for atom in atoms:
	data.append(get_NMR(atom))


#print to output file
with open(file_output, 'a', newline='') as csvfile:
	thewriter = csv.writer(csvfile)
	thewriter.writerow(data)
