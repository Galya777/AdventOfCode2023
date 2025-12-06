package Day22;

public record Point3D(int x, int y, int z) implements Comparable<Point3D> {
    @Override
    public int compareTo(Point3D o) {
        if (this.z != o.z) return Integer.compare(this.z, o.z);
        if (this.y != o.y) return Integer.compare(this.y, o.y);
        return Integer.compare(this.x, o.x);
    }

    public Point3D moveDown() {
        return new Point3D(x, y, z - 1);
    }

    public Point3D moveUp() {
        return new Point3D(x, y, z + 1);
    }
}
