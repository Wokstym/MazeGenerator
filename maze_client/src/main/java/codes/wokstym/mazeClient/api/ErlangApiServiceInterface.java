package codes.wokstym.mazeClient.api;

import codes.wokstym.mazeClient.MazeStructure.Maze;
import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangRangeException;

import java.io.IOException;

public interface ErlangApiServiceInterface {


    Maze getNewMaze() throws OtpErlangDecodeException, OtpErlangExit, OtpAuthException, IOException, OtpErlangRangeException;

    void createEmptyMaze(int Height, int Width) throws OtpErlangDecodeException, OtpErlangExit, OtpAuthException, IOException;

    Maze step(Maze maze) throws OtpErlangDecodeException, OtpErlangExit, IOException, OtpAuthException;
}
