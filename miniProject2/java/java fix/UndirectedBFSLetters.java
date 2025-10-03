import java.io.*;
import java.util.*;

public class UndirectedBFSLetters {
    public static void main(String[] args) throws Exception {
        FastInput in = new FastInput(System.in);

        int N = in.nextInt();
        int M = in.nextInt();

        // Read SRC/DST as labels
        String srcLabel = in.next();
        String dstLabel = in.next();

        List<String[]> edgeLabels = new ArrayList<>(M);
        TreeSet<String> allLabels = new TreeSet<>();
        allLabels.add(srcLabel);
        allLabels.add(dstLabel);

        for (int i = 0; i < M; i++) {
            String u = in.next();
            String v = in.next();
            edgeLabels.add(new String[]{u, v});
            allLabels.add(u);
            allLabels.add(v);
        }

        if (allLabels.size() != N) {
            N = allLabels.size(); // adjust if mismatch
        }

        // Label ↔ index mapping (alphabetical order)
        Map<String,Integer> id = new HashMap<>(N*2);
        String[] labelOf = new String[N+1];
        int idx = 1;
        for (String s : allLabels) {
            id.put(s, idx);
            labelOf[idx] = s;
            idx++;
        }

        Integer SRC = id.get(srcLabel);
        Integer DST = id.get(dstLabel);
        if (SRC == null || DST == null) {
            System.out.println("No path");
            return;
        }

        @SuppressWarnings("unchecked")
        TreeSet<Integer>[] neigh = new TreeSet[N+1];
        for (int i=1;i<=N;i++) neigh[i] = new TreeSet<>();

        // Add undirected edges
        for (String[] e : edgeLabels) {
            Integer u = id.get(e[0]);
            Integer v = id.get(e[1]);
            if (u != null && v != null) {
                neigh[u].add(v);
                neigh[v].add(u); // add reverse edge
            }
        }

        int[][] adj = new int[N+1][];
        for (int i=1;i<=N;i++) {
            adj[i] = neigh[i].stream().mapToInt(Integer::intValue).toArray();
        }

        int[] parent = bfsParent(N, adj, SRC, DST);
        if (parent == null) {
            System.out.println("No path");
        } else {
            List<Integer> pathIdx = reconstruct(parent, SRC, DST);
            if (pathIdx.isEmpty() || pathIdx.get(0) != SRC) {
                System.out.println("No path");
            } else {
                StringBuilder sb = new StringBuilder();
                for (int i=0;i<pathIdx.size();i++) {
                    if (i > 0) sb.append(' ');
                    sb.append(labelOf[pathIdx.get(i)]);
                }
                System.out.println(sb.toString());
            }
        }
    }

    private static int[] bfsParent(int N, int[][] adj, int SRC, int DST) {
        if (SRC < 1 || SRC > N || DST < 1 || DST > N) return null;
        boolean[] vis = new boolean[N+1];
        int[] parent = new int[N+1];
        Arrays.fill(parent, -1);
        ArrayDeque<Integer> q = new ArrayDeque<>();
        q.add(SRC);
        vis[SRC] = true;
        while (!q.isEmpty()) {
            int u = q.remove();
            if (u == DST) return parent;
            for (int v : adj[u]) {
                if (!vis[v]) {
                    vis[v] = true;
                    parent[v] = u;
                    q.add(v);
                }
            }
        }
        return null;
    }

    private static List<Integer> reconstruct(int[] parent, int SRC, int DST) {
        ArrayList<Integer> path = new ArrayList<>();
        for (int cur = DST; cur != -1; cur = parent[cur]) path.add(cur);
        Collections.reverse(path);
        return path;
    }

    // Input reader
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
