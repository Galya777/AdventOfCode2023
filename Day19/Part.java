package Day19;

public record Part(int x, int m, int a, int s) {
    public static Part fromString(String line) {
        // Format: {x=787,m=2655,a=1222,s=2876}
        String[] parts = line.substring(1, line.length() - 1).split(",");
        int x = Integer.parseInt(parts[0].split("=")[1]);
        int m = Integer.parseInt(parts[1].split("=")[1]);
        int a = Integer.parseInt(parts[2].split("=")[1]);
        int s = Integer.parseInt(parts[3].split("=")[1]);
        return new Part(x, m, a, s);
    }
    
    public int getRatingSum() {
        return x + m + a + s;
    }
}
