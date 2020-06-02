package codes.wokstym.mazeClient.api;


import codes.wokstym.mazeClient.utils.Maze;
import codes.wokstym.mazeClient.utils.Position;
import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class MazeApi implements MazeApiInterface {

    private final OtpConnection connection;

    public MazeApi(String cookie, String serverNodeName) throws IOException, OtpAuthException {
        OtpSelf client = new OtpSelf("mazeClientNode", cookie);
        OtpPeer server = new OtpPeer(serverNodeName);
        this.connection = client.connect(server);
    }

    public Maze updateMaze(Maze maze) throws Exception {

        OtpErlangTuple resultTuple = getMazeData();
        OtpErlangMap positions = (OtpErlangMap) resultTuple.elementAt(3);
        List<Position> positionHashSet = parseErlMap(positions);

        maze.setPositions(positionHashSet);
        return maze;

    }

    public Maze getNewMaze() throws Exception {

        OtpErlangTuple resultTuple = getMazeData();

        int width = ((OtpErlangLong) resultTuple.elementAt(1)).intValue();
        int height = ((OtpErlangLong) resultTuple.elementAt(2)).intValue();
        OtpErlangMap positions = (OtpErlangMap) resultTuple.elementAt(3);

        List<Position> positionHashSet = parseErlMap(positions);
        return new Maze(width, height, positionHashSet);
    }


    public void createEmptyMaze(int Height, int Width) throws Exception {

        OtpErlangObject[] args = new OtpErlangObject[]{
                new OtpErlangInt(Height),
                new OtpErlangInt(Width)
        };

        connection.sendRPC("maze_api_gen_server", "createEmptyMaze", args);

        checkIfResponseIsOk();
    }

    private OtpErlangTuple getMazeData() throws Exception {
        connection.sendRPC("maze_api_gen_server", "getMonitor", new OtpErlangObject[]{});

        OtpErlangTuple response = (OtpErlangTuple) connection.receiveMsg().getMsg();
        System.out.println(response);
        OtpErlangObject responseVal = response.elementAt(1);
        OtpErlangTuple resultTuple = (OtpErlangTuple) responseVal;

        checkIfIsMaze(resultTuple);
        return resultTuple;
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


    private void checkIfResponseIsOk() throws Exception {
        OtpErlangTuple responseTuple = (OtpErlangTuple) connection.receiveMsg().getMsg();
        OtpErlangObject responseVal = responseTuple.elementAt(1);
        if (!((responseVal instanceof OtpErlangAtom) &&
                ((OtpErlangAtom) responseVal).atomValue().equals("ok"))) {
            throw new Exception("server returned error: " + responseVal);
        }
    }

    private void checkIfIsMaze(OtpErlangTuple resultTuple) throws Exception {

        OtpErlangAtom resultType = (OtpErlangAtom) resultTuple.elementAt(0);
        if (!(resultType
                .atomValue()
                .equals("maze") &&
                resultTuple.elements().length == 4)) {
            throw new Exception("server returned error: " + resultTuple);
        }
    }
}
