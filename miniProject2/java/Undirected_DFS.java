import java.io.*;
import java.util.*;

public class UndirectedDFS {
    public static void main(String[] args) throws Exception {
        FastInput in = new FastInput(System.in);

        int N = in.nextInt();
        int M = in.nextInt();
        int SRC = in.nextInt();
        int DST = in.nextInt();

        @SuppressWarnings("unchecked")
        TreeSet<Integer>[] neigh = new TreeSet[N + 1];
        for (int i = 1; i <= N; i++) neigh[i] = new TreeSet<>();

        for (int i = 0; i < M; i++) {
            int u = in.nextInt();
            int v = in.nextInt();
            if (1 <= u && u <= N) neigh[u].add(v);
            if (1 <= v && v <= N) neigh[v].add(u); // add reverse edge
        }

        int[][] adj = new int[N + 1][];
        for (int i = 1; i <= N; i++) {
            adj[i] = neigh[i].stream().distinct().sorted().mapToInt(Integer::intValue).toArray();
        }

        String out = dfsPath(N, adj, SRC, DST);
        System.out.println(out);
    }

    private static String dfsPath(int N, int[][] adj, int SRC, int DST) {
        if (SRC < 1 || SRC > N || DST < 1 || DST > N) return "No path";
        boolean[] vis = new boolean[N + 1];
        ArrayList<Integer> path = new ArrayList<>();
        if (!dfs(SRC, DST, adj, vis, path)) return "No path";

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < path.size(); i++) {
            if (i > 0) sb.append(' ');
            sb.append(path.get(i));
        }
        return sb.toString();
    }

    // preorder: mark visited on entry; first-found path under sorted neighbor order
    private static boolean dfs(int u, int DST, int[][] adj, boolean[] vis, ArrayList<Integer> path) {
        vis[u] = true;
        path.add(u);
        if (u == DST) return true;
        for (int v : adj[u]) {
            if (1 <= v && v < vis.length && !vis[v]) {
                if (dfs(v, DST, adj, vis, path)) return true;
            }
        }
        path.remove(path.size() - 1);
        return false;
    }

    static final class FastInput {
        BufferedReader br;
        StringTokenizer st;
        FastInput(InputStream is) { br = new BufferedReader(new InputStreamReader(is)); }
        String next() throws IOException {
            while (st == null || !st.hasMoreElements()) {
                String line = br.readLine();
                if (line == null) return null;
                if (line.trim().isEmpty()) continue;
                st = new StringTokenizer(line);
            }
            return st.nextToken();
        }
        int nextInt() throws IOException {
            String s = next();
            if (s == null) throw new EOFException();
            return Integer.parseInt(s);
        }
    }
}
