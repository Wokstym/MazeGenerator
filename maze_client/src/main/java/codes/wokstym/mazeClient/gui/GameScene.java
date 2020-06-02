package codes.wokstym.mazeClient.gui;

import codes.wokstym.mazeClient.utils.Position;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import javafx.scene.shape.Rectangle;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.Group;
import javafx.scene.Scene;
import lombok.Getter;
import lombok.SneakyThrows;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static codes.wokstym.mazeClient.gui.GameFrame.PIXEL_SIZE;


/**
 * Scene which holds cell rectangles and is responsible for changing
 * colors depending or them being alive or not
 */
class GameScene extends Scene {

    @Getter
    private Map<Position, Rectangle> rectangles;
    private  Group group;

    /**
     * Generate black rectangles, store them in a HasMap and add
     * them to scene
     *  @param rootPane     rootPane which stores objects shown
     * @param height       height of scene
     * @param width        width of scene
     * @param positionList list of OtpErlangTuple representing walls positions
     */
    GameScene(Pane rootPane, int height, int width, List<Position> positionList) {
        super(rootPane, width, height, Color.BLACK);

        group = new Group();

        rectangles = positionList
                .stream()
                .collect(
                        Collectors.toMap(
                                position -> position,  position ->this.genRecAndAddTo(group,position)
                        )
                );
        rootPane.getChildren().add(group);
    }

    /**
     * For each rectangle checks if cell, corresponding to it, is alive
     * and based on that set its color
     *
     * @param positionList list of OtpErlangTuple representing walls positions
     */
    void refreshScene(List<Position> positionList) {
        positionList.stream()
                .filter(position -> !this.rectangles.containsKey(position))
                .forEach(position -> this.rectangles.put(position,this.genRecAndAddTo(this.group,position)));
    }

    private Rectangle genRecAndAddTo(Group group, Position position){
        Rectangle rec =  generateRectangle(position);
        group.getChildren().add(rec);
        return rec;
    }

    @SneakyThrows
    private Rectangle generateRectangle(Position boardPos) {
        Position canvasPos = getRoomPosOnCanvas(boardPos);
        Rectangle rectangle = new Rectangle(PIXEL_SIZE, PIXEL_SIZE);
        rectangle.setX(canvasPos.x);
        rectangle.setY(canvasPos.y);
        rectangle.setFill(Color.WHITE);
        return rectangle;
    }

    private Position getRoomPosOnCanvas(Position relativeRoomPos) throws OtpErlangRangeException {
        int xPosOnCanvas = relativeRoomPos.x * PIXEL_SIZE;
        int yPosOnCanvas = relativeRoomPos.y * PIXEL_SIZE;
        return new Position(xPosOnCanvas, yPosOnCanvas);
    }

}