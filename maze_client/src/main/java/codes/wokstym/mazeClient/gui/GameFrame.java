package codes.wokstym.mazeClient.gui;

import codes.wokstym.mazeClient.api.MazeApi;
import codes.wokstym.mazeClient.api.MazeApiInterface;
import codes.wokstym.mazeClient.utils.Maze;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;

import java.util.Timer;
import java.util.TimerTask;

public class GameFrame extends Application {

    static final int PIXEL_SIZE = 8;
    static final int FREQUENCY = 15;

    public static void main(String[] args) {
        launch(args);
    }

    /**
     * Main function for setting basic settings, declaring key event on Enter
     * and starting application timer
     */
    @Override
    public void start(Stage stage) throws Exception {



        MazeApiInterface mazeApi = new MazeApi("erljava", "apiNode");
//        mazeApi.createEmptyMaze(100, 100);
        Maze maze = mazeApi.getNewMaze();

        stage.setTitle("Maze Generator");
        stage.setResizable(false);
        stage.setOnCloseRequest(event -> Platform.exit());


        int stageHeight = maze.height * PIXEL_SIZE;
        int stageWidth = maze.width * PIXEL_SIZE;

        Pane rootNode = new Pane();
        GameScene gameScene = new GameScene(rootNode, stageHeight, stageWidth, maze.getPositions());
        stage.setScene(gameScene);
        stage.show();
        System.out.println(gameScene.getRectangles());

        new Timer().scheduleAtFixedRate(new TimerTask() {
            public void run() {
                Platform.runLater(() -> {
                   /* boardOperator.tick();
                    gameScene.refreshScene(boardOperator.getCurrentPatternBoard().getAllCells());*/
                });
            }
        }, 0, 1000 / FREQUENCY);
    }
}
