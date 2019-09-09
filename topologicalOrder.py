import numpy as np

def topologicalOrder(incidence):
    orderedEdges = [] # declare empty ordered list
    noFanIn = [] # create empty fan-in vector that will function as a Queu

    # Determine the fan-in
    incidenceColumnSums = np.sum(incidence, axis = 0)
    # Loop through all edges
    idx = 0
    for colSum in incidenceColumnSums:
        idx += 1
        # Check if orphaned node ie colsums == 0 
        if colSum == 0:
            noFanIn.append(idx)
    
    # Main while loop
    idx = 0
    while len(noFanIn) > 0:
        v = noFanIn[idx] # V is the first element of no fan in
        noFanIn.pop(0) # Remove the first element from the noFanIn List

        orderedEdges.append(v) # append the v node
        # Check for the childs of the v node to append to noFanIn
        for jdx in range(incidence.shape[1]):
            currEdge = incidence[v - 1, jdx]
            if currEdge == 1:
                incidenceColumnSums[jdx] = incidenceColumnSums[jdx] - 1
                # Check if all parents of the current node j are already ordered
                if incidenceColumnSums[jdx] == 0:
                    # Append to the noFanInSet thats the Queu
                    noFanIn.append(jdx + 1)
    # Returnerino
    return orderedEdges

    

