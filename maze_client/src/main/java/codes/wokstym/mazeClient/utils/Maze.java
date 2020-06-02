package codes.wokstym.mazeClient.utils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@AllArgsConstructor
@Getter
public class Maze {

    public final int width;
    public final int height;
    @Setter
    private List<Position> positions;

}
