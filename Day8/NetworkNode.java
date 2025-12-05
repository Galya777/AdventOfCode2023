package Day8;

public class NetworkNode {
    private final String name;
    private NetworkNode left;
    private NetworkNode right;
    private final String leftNodeName;
    private final String rightNodeName;

    public NetworkNode(String name, String leftNodeName, String rightNodeName) {
        this.name = name;
        this.leftNodeName = leftNodeName;
        this.rightNodeName = rightNodeName;
    }

    public String getName() {
        return name;
    }

    public NetworkNode getLeft() {
        return left;
    }

    public void setLeft(NetworkNode left) {
        this.left = left;
    }

    public NetworkNode getRight() {
        return right;
    }

    public void setRight(NetworkNode right) {
        this.right = right;
    }

    public String getLeftNodeName() {
        return leftNodeName;
    }

    public String getRightNodeName() {
        return rightNodeName;
    }
}
