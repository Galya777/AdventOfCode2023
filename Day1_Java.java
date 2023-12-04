public class hey {

    public static char[][] read() throws IOException {
        BufferedReader bo = new BufferedReader(new FileReader("input.txt"));
        int column = Integer.parseInt(bo.readLine());
        int row = Integer.parseInt(bo.readLine());
        char[][] map = new char[row][column];
        for (int i = 0; i < row; i++) {
            String line = bo.readLine();
            for (int j = 0; j < column; j++) {
                map[i][j] = line.charAt(j);
            }
        }
        bo.close();
        return map;
    }
}
class HelloWorld {
    public static void main(String[] args) {
       char[][] data=hey.read();
       int sum=0;
       int num=0;
       for(int i=0; i<data.length; ++i){
      for(int j=0; j<data[i].length; ++j){
          int counter=1;
           if(isDigit(data[i][j])){
               num= Character.getNumericValue(data[i][j])+10*counter;
               --counter;
           }
       }
       sum+=num;
       }
       System.out.println(sum);
    }
}
