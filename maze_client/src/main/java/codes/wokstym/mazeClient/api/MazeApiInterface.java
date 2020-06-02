package codes.wokstym.mazeClient.api;

import codes.wokstym.mazeClient.utils.Maze;

public interface MazeApiInterface {

    public Maze updateMaze(Maze maze) throws Exception;
    public Maze getNewMaze() throws Exception;
    public void createEmptyMaze(int Height, int Width) throws Exception;
}
