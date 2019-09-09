import numpy as np

def main():
    # Declare the incidence matrix that we are going to work
    incidence = np.array([
        [0, 1, 1, 0, 0, 0],
        [0, 0, 1, 0, 0, 0],
        [0, 0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0, 0],
        [0, 0, 0, 1, 0, 0],
        [0, 0, 0, 1, 1, 0]
    ])

    iMatrix = np.identity(6)

    ones = np.ones((6,6))

    print(incidence.shape)

if __name__ == '__main__':
    main()
