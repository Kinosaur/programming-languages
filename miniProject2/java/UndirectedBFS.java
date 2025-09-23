import java.io.*;
import java.util.*;

public class UndirectedBFS {
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

        String out = bfsPath(N, adj, SRC, DST);
        System.out.println(out);
    }

    private static String bfsPath(int N, int[][] adj, int SRC, int DST) {
        if (SRC < 1 || SRC > N || DST < 1 || DST > N) return "No path";
        boolean[] vis = new boolean[N + 1];
        int[] parent = new int[N + 1];
        Arrays.fill(parent, -1);
        ArrayDeque<Integer> q = new ArrayDeque<>();
        q.add(SRC);
        vis[SRC] = true; // mark on enqueue

        while (!q.isEmpty()) {
            int u = q.remove();     // stop when DST is dequeued
            if (u == DST) return reconstruct(parent, SRC, DST);
            for (int v : adj[u]) {
                if (1 <= v && v <= N && !vis[v]) {
                    vis[v] = true;
                    parent[v] = u;
                    q.add(v);
                }
            }
        }
        return "No path";
    }

    private static String reconstruct(int[] parent, int SRC, int DST) {
        ArrayList<Integer> path = new ArrayList<>();
        for (int cur = DST; cur != -1; cur = parent[cur]) path.add(cur);
        Collections.reverse(path);
        if (path.isEmpty() || path.get(0) != SRC) return "No path";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < path.size(); i++) {
            if (i > 0) sb.append(' ');
            sb.append(path.get(i));
        }
        return sb.toString();
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
