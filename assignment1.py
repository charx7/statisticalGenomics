import numpy as np

def edgeAdditionsValidity(incidence, ancestor, iMatrix, ones):
    # check for the validity of edge addition operations
    addition_safe = (ones - iMatrix - incidence) - ancestor

    return addition_safe

def edgeReversalValidity(incidence, ancestor):
    # Check for the validity of edge reversal
    reversal_safe = incidence - (np.matmul(incidence.T, ancestor)).T
    # if the elements are != than 1 then mask
    reversal_safe[reversal_safe != 1] = 0
    
    return reversal_safe

def main():
    # Declare the incidence/ancestor matrix that we are going to work
    incidence = np.array([
        [0, 1, 1, 0, 0, 0],
        [0, 0, 1, 0, 0, 0],
        [0, 0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0, 0],
        [0, 0, 0, 1, 0, 0],
        [0, 0, 0, 1, 1, 0]
    ])

    ancestor = np.array([
        [0, 0, 0, 0, 0, 0],
        [1, 0, 0, 1, 1, 1],
        [1, 1, 0, 1, 1, 1],
        [0, 0, 0, 0, 1, 1],
        [0, 0, 0, 0, 0, 1],
        [0, 0, 0, 0, 0, 0]
    ])

    iMatrix = np.identity(6)

    ones = np.ones((6,6))

    # Check for the validity of edge additions
    addValid = edgeAdditionsValidity(incidence, ancestor, iMatrix, ones)
    print('The valid edge additions operations matrix are: \n' , addValid)

    # Check for the validity of edge reversals
    reversalValid = edgeReversalValidity(incidence, ancestor)
    print('The valid edge reversal operations matrix is: \n', reversalValid)

    # Now we calculate the total number of neighbour graphs
    validSumMatrix = addValid + reversalValid + incidence
    neighGraphs = np.sum(validSumMatrix)
    print('We can reach a total of: ',neighGraphs, ' neigh graphs.')


if __name__ == '__main__':
    main()
