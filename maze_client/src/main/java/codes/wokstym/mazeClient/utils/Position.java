package codes.wokstym.mazeClient.utils;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Representation of position on a two dimensional plane
 */
@Data
@EqualsAndHashCode
public class Position {

    final public int x;
    final public int y;

    /**
     * Tests whether this Position is in a boundary
     *
     * @param upperLeft The upper left corner of the boundary
     * @param lowerRight The lower right corner of the boundary
     * @return true if Position is in a boundary, false otherwise
     */
    public Boolean isInBoundary(Position upperLeft, Position lowerRight) {
        return this.follows(upperLeft) && this.precedes(lowerRight);
    }

    /**
     * Generates list of Positions in a boundary without repetitions
     *
     * @param n Number of positions that is generated
     * @param upperLeft The upper left corner of the boundary in which the positions is generated
     * @param lowerRight The lower right corner of the boundary in which the positions is generated
     * @return List of Positions generated
     */
    public static List<Position> genRandomInBoundary(int n, Position upperLeft, Position lowerRight) {

        /* adds all possible spaces to list, shuffle it and returns subList of length n */
        List<Position> emptySpaces = new ArrayList<>();
        for (int i = upperLeft.x; i <= lowerRight.x; i++) {
            for (int j = upperLeft.y; j <= lowerRight.y; j++) {
                emptySpaces.add(new Position(i, j));
            }
        }
        Collections.shuffle(emptySpaces);
        return emptySpaces.subList(0, n);
    }

    /**
     * Generate neighbouring positions to this position. Neighbour is a position that is horizontally,
     * vertically, or diagonally adjacent.
     *
     * @return List of 8 neighboring positions
     */
    public List<Position> getNeighbours() {
        return neighboursDifferences
                .stream()
                .map(differencePos -> differencePos.genAddedPos(this))
                .collect(Collectors.toList());
    }


    /* constant List of 8 differences that are used to calculate position of neighbours by addition */
    private static final List<Position> neighboursDifferences = Arrays.asList(
            new Position(-1, -1),
            new Position(0, -1),
            new Position(1, -1),
            new Position(1, 0),
            new Position(1, 1),
            new Position(0, 1),
            new Position(-1, 1),
            new Position(-1, 0));


    private boolean precedes(Position other) {
        return this.x <= other.x && this.y <= other.y;
    }

    private boolean follows(Position other) {
        return this.x >= other.x && this.y >= other.y;
    }

    private Position genAddedPos(Position other) {
        return new Position(x + other.x, y + other.y);
    }
}