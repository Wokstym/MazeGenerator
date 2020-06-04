package codes.wokstym.mazeClient.api;

import codes.wokstym.mazeClient.MazeStructure.Maze;
import codes.wokstym.mazeClient.MazeStructure.Position;
import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ErlangApiService implements ErlangApiServiceInterface {

    private final OtpConnection connection;
    private boolean isFinished = false;

    public ErlangApiService(String cookie, String serverNodeName) throws IOException, OtpAuthException {
        OtpSelf client = new OtpSelf("mazeClientNode", cookie);
        OtpPeer server = new OtpPeer(serverNodeName);
        this.connection = client.connect(server);
    }


    public Maze getNewMaze() throws OtpErlangDecodeException, OtpErlangExit, OtpAuthException, IOException, OtpErlangRangeException {
        connection.sendRPC("maze_api_gen_server", "getMaze", new OtpErlangObject[]{});

        OtpErlangTuple response = (OtpErlangTuple) connection.receiveMsg().getMsg();
        OtpErlangTuple responseTuple = (OtpErlangTuple) response.elementAt(1);
        OtpErlangObject responseVal = responseTuple.elementAt(1);
        OtpErlangTuple resultTuple = (OtpErlangTuple) responseVal;

        checkIfIsMaze(resultTuple);

        int width = ((OtpErlangLong) resultTuple.elementAt(2)).intValue();
        int height = ((OtpErlangLong) resultTuple.elementAt(1)).intValue();
        OtpErlangMap positions = (OtpErlangMap) resultTuple.elementAt(3);

        List<Position> positionHashSet = parseErlMap(positions);
        return new Maze(width, height, positionHashSet);
    }

    public void createEmptyMaze(int Height, int Width) throws OtpErlangDecodeException, OtpErlangExit, OtpAuthException, IOException {

        OtpErlangObject[] args = new OtpErlangObject[]{
                new OtpErlangInt(Height),
                new OtpErlangInt(Width)
        };

        connection.sendRPC("maze_api_gen_server", "createEmptyMaze", args);

        checkIfResponseIsOk();
    }

    public Maze step(Maze maze) throws OtpErlangDecodeException, OtpErlangExit, IOException, OtpAuthException {
        if (this.isFinished)
            return maze;

        connection.sendRPC("maze_api_gen_server", "step", new OtpErlangObject[]{});
        OtpErlangTuple response = (OtpErlangTuple) connection.receiveMsg().getMsg();
        OtpErlangTuple responseTuple = (OtpErlangTuple) response.elementAt(1);
        if (responseTuple.elementAt(0).toString().equals("finished_generation")) {
            System.out.println("Finished generation");
            this.isFinished = true;
            return maze;
        }

        OtpErlangTuple resultTuple = (OtpErlangTuple) responseTuple.elementAt(1);
        checkIfIsMaze(resultTuple);
        OtpErlangMap positions = (OtpErlangMap) resultTuple.elementAt(3);

        List<Position> positionHashSet = parseErlMap(positions);

        maze.setPositions(positionHashSet);
        return maze;
    }


    private List<Position> parseErlMap(OtpErlangMap positions) {
        return positions
                .entrySet()
                .stream()
                .map(Map.Entry::getKey)
                .map(posObj -> (OtpErlangTuple) posObj)
                .map(posTupl -> {
                    int x = 0;
                    int y = 0;
                    try {
                        x = ((OtpErlangLong) posTupl.elementAt(0)).intValue();
                        y = ((OtpErlangLong) posTupl.elementAt(1)).intValue();
                    } catch (OtpErlangRangeException e) {
                        e.printStackTrace();
                    }
                    return new Position(x, y);
                })
                .collect(Collectors.toList());
    }


    private void checkIfResponseIsOk() throws OtpErlangExit, IOException, OtpAuthException, OtpErlangDecodeException {
        OtpErlangTuple responseTuple = (OtpErlangTuple) connection.receiveMsg().getMsg();
        OtpErlangObject responseVal = responseTuple.elementAt(1);
        if (!(responseVal instanceof OtpErlangAtom &&
                ((OtpErlangAtom) responseVal).atomValue().equals("ok")))
            throw new OtpErlangDecodeException("Server returned error: " + responseVal);

    }

    private void checkIfIsMaze(OtpErlangTuple resultTuple) throws OtpErlangDecodeException {

        OtpErlangAtom resultType = (OtpErlangAtom) resultTuple.elementAt(0);
        if (!(resultType
                .atomValue()
                .equals("maze") &&
                resultTuple.elements().length == 4)) {
            throw new OtpErlangDecodeException("Server returned error: " + resultTuple);
        }
    }
}
