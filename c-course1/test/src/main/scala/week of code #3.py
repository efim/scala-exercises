import sys


def is_lone_int(sequence, index, i):
    if (index == 0 or index >= (len(sequence)-1)):
        return False
    else:
        return sequence[index] == i and sequence[index - 1] == 0 and sequence[index + 1] == 0

def calculateGameLength(sequence):
    loneOnes = [x for index, x in enumerate(sequence) if is_lone_int(sequence, index, 1)]
    print(loneOnes)
    loneOnesCount = len(loneOnes)
    restState = [x for index, x in enumerate(sequence) if not is_lone_int(sequence,index, 1)]
    print(restState)

    restMoves = [x for index, x in enumerate(restState) if is_lone_int(restState, index, 0)]
    print(restMoves)
    restMovesCount = len(restMoves)
    return restMovesCount+ loneOnesCount

def isWinningPosition(sequence):
    return calculateGameLength(sequence) % 2 == 1

g = int(input().strip())
for a0 in range(g):
    n = int(input().strip())
    sequence = list(map(int, input().strip().split(' ')))
    if (isWinningPosition(sequence)):
        print("Alice")
    else:
        print("Bob")

# print(calculateGameLength([0,1,1,1,1,0]))
# print(calculateGameLength([1,1,1,0,1]))
# print(calculateGameLength([1,1,0,0,0,0,1]))
# print(calculateGameLength([1,0,0,0,0,0,1,0,0,0]))