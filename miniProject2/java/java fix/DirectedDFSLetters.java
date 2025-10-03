import java.io.*;
import java.util.*;

/** DFS on a DIRECTED graph with letter node IDs. */
public class DirectedDFSLetters {
    public static void main(String[] args) {
        try {
            FastInput in = new FastInput(System.in);

            // Read N, M
            Integer NBox = in.nextIntOrNull();
            Integer MBox = in.nextIntOrNull();
            if (NBox == null || MBox == null) { System.out.println("No path"); return; }
            int N = NBox, M = MBox;

            // Read SRC, DST as labels
            String srcLabel = in.next();
            String dstLabel = in.next();
            if (srcLabel == null || dstLabel == null) { System.out.println("No path"); return; }

            // Read M edges as labels
            List<String[]> edgeLabels = new ArrayList<>(M);
            TreeSet<String> allLabels = new TreeSet<>();
            allLabels.add(srcLabel);
            allLabels.add(dstLabel);

            for (int i = 0; i < M; i++) {
                String u = in.next();
                String v = in.next();
                if (u == null || v == null) { System.out.println("No path"); return; }
                edgeLabels.add(new String[]{u, v});
                allLabels.add(u);
                allLabels.add(v);
            }

            if (allLabels.size() != N) N = allLabels.size();
            Map<String,Integer> id = new HashMap<>();
            String[] labelOf = new String[N+1];
            int idx = 1;
            for (String s : allLabels) { id.put(s, idx); labelOf[idx] = s; idx++; }

            Integer SRC = id.get(srcLabel), DST = id.get(dstLabel);
            if (SRC == null || DST == null) { System.out.println("No path"); return; }

            @SuppressWarnings("unchecked")
            TreeSet<Integer>[] neigh = new TreeSet[N+1];
            for (int i=1;i<=N;i++) neigh[i] = new TreeSet<>();

            for (String[] e : edgeLabels) {
                Integer u = id.get(e[0]);
                Integer v = id.get(e[1]);
                if (u==null||v==null) continue;
                neigh[u].add(v); // directed
            }

            int[][] adj = new int[N+1][];
            for (int i=1;i<=N;i++) {
                adj[i] = neigh[i].stream().mapToInt(Integer::intValue).toArray();
            }

            boolean[] vis = new boolean[N+1];
            List<Integer> path = new ArrayList<>();
            boolean found = dfs(SRC, DST, adj, vis, path);
            if (!found) { System.out.println("No path"); return; }

            StringBuilder sb = new StringBuilder();
            for (int i=0;i<path.size();i++) {
                if (i>0) sb.append(' ');
                sb.append(labelOf[path.get(i)]);
            }
            System.out.println(sb.toString());
        } catch (Exception e) {
            System.out.println("No path");
        }
    }

    private static boolean dfs(int u, int DST, int[][] adj, boolean[] vis, List<Integer> path) {
        vis[u] = true;
        path.add(u);
        if (u==DST) return true;
        for (int v: adj[u]) {
            if (!vis[v]) {
                List<Integer> copy = new ArrayList<>(path);
                boolean found = dfs(v, DST, adj, vis, copy);
                if (found) {
                    path.clear();
                    path.addAll(copy);
                    return true;
                }
            }
        }
        return false;
    }

    static final class FastInput {
        BufferedReader br; StringTokenizer st;
        FastInput(InputStream is) { br = new BufferedReader(new InputStreamReader(is)); }
        String next() throws IOException {
            while (st==null||!st.hasMoreElements()) {
                String line=br.readLine();
                if (line==null) return null;
                if (line.trim().isEmpty()) continue;
                st=new StringTokenizer(line);
            }
            return st.nextToken();
        }
        Integer nextIntOrNull() throws IOException {
            String s=next();
            if (s==null) return null;
            return Integer.parseInt(s);
        }
    }
}
