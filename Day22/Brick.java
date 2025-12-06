package Day22;

import java.util.*;
import java.util.stream.Collectors;

public class Brick implements Comparable<Brick> {
    public final Point3D start;
    public final Point3D end;
    public final Set<Brick> supportedBy = new HashSet<>();
    public final Set<Brick> supports = new HashSet<>();
    private Set<Point3D> cubes;

    public Brick(Point3D start, Point3D end) {
        this.start = start;
        this.end = end;
    }

    public static Brick fromString(String line) {
        String[] parts = line.split("~");
        String[] startCoords = parts[0].split(",");
        String[] endCoords = parts[1].split(",");

        Point3D start = new Point3D(
                Integer.parseInt(startCoords[0]),
                Integer.parseInt(startCoords[1]),
                Integer.parseInt(startCoords[2])
        );

        Point3D end = new Point3D(
                Integer.parseInt(endCoords[0]),
                Integer.parseInt(endCoords[1]),
                Integer.parseInt(endCoords[2])
        );

        // Ensure start is the lower coordinate
        if (start.compareTo(end) > 0) {
            Point3D temp = start;
            start = end;
            end = temp;
        }

        return new Brick(start, end);
    }

    public Set<Point3D> getCubes() {
        if (cubes == null) {
            cubes = new HashSet<>();
            int dx = Integer.signum(end.x() - start.x());
            int dy = Integer.signum(end.y() - start.y());
            int dz = Integer.signum(end.z() - start.z());

            int x = start.x();
            int y = start.y();
            int z = start.z();

            while (true) {
                cubes.add(new Point3D(x, y, z));
                if (x == end.x() && y == end.y() && z == end.z()) break;

                if (x != end.x()) x += dx;
                if (y != end.y()) y += dy;
                if (z != end.z()) z += dz;
            }
        }
        return cubes;
    }

    public Brick moveDown() {
        return new Brick(
                new Point3D(start.x(), start.y(), start.z() - 1),
                new Point3D(end.x(), end.y(), end.z() - 1)
        );
    }

    public boolean isAtGround() {
        return Math.min(start.z(), end.z()) == 1;
    }

    @Override
    public int compareTo(Brick other) {
        int thisMinZ = Math.min(start.z(), end.z());
        int otherMinZ = Math.min(other.start.z(), other.end.z());
        if (thisMinZ != otherMinZ) return Integer.compare(thisMinZ, otherMinZ);

        int thisMaxZ = Math.max(start.z(), end.z());
        int otherMaxZ = Math.max(other.start.z(), other.end.z());
        if (thisMaxZ != otherMaxZ) return Integer.compare(thisMaxZ, otherMaxZ);

        int thisMinY = Math.min(start.y(), end.y());
        int otherMinY = Math.min(other.start.y(), other.end.y());
        if (thisMinY != otherMinY) return Integer.compare(thisMinY, otherMinY);

        int thisMaxY = Math.max(start.y(), end.y());
        int otherMaxY = Math.max(other.start.y(), other.end.y());
        if (thisMaxY != otherMaxY) return Integer.compare(thisMaxY, otherMaxY);

        int thisMinX = Math.min(start.x(), end.x());
        int otherMinX = Math.min(other.start.x(), other.end.x());
        if (thisMinX != otherMinX) return Integer.compare(thisMinX, otherMinX);

        int thisMaxX = Math.max(start.x(), end.x());
        int otherMaxX = Math.max(other.start.x(), other.end.x());
        return Integer.compare(thisMaxX, otherMaxX);
    }

    @Override
    public String toString() {
        return String.format("Brick[%s~%s]", start, end);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Brick brick = (Brick) o;
        return Objects.equals(start, brick.start) && Objects.equals(end, brick.end);
    }

    @Override
    public int hashCode() {
        return Objects.hash(start, end);
    }
}