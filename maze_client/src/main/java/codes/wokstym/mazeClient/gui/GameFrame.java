package codes.wokstym.mazeClient.gui;

import codes.wokstym.mazeClient.api.ErlangApiService;
import codes.wokstym.mazeClient.api.ErlangApiServiceInterface;
import codes.wokstym.mazeClient.MazeStructure.Maze;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicReference;

public class GameFrame extends Application {

    static final int PIXEL_SIZE = 8;
    static final int FREQUENCY = 144;
    static final int MAZE_HEIGHT = 81;
    static final int MAZE_WIDTH = 81;

    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage stage) throws Exception {


        ErlangApiServiceInterface mazeApi = new ErlangApiService("erljava", "apiNode");
        mazeApi.createEmptyMaze(MAZE_HEIGHT, MAZE_WIDTH);
        AtomicReference<Maze> maze = new AtomicReference<>(mazeApi.getNewMaze());


        stage.setTitle("Maze Generator");
        stage.setResizable(false);
        stage.setOnCloseRequest(event -> Platform.exit());


        int stageHeight = maze.get().height * PIXEL_SIZE;
        int stageWidth = maze.get().width * PIXEL_SIZE;

        Pane rootNode = new Pane();
        GameScene gameScene = new GameScene(rootNode, stageHeight, stageWidth, maze.get().getPositions());
        stage.setScene(gameScene);
        stage.show();

        new Timer().schedule(new TimerTask() {
            public void run() {
                Platform.runLater(() -> {
                    try {
                        maze.set(mazeApi.step(maze.get()));
                        gameScene.refreshScene(maze.get().getPositions());
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                });
            }
        }, 0, 1000 / FREQUENCY);
    }
}
