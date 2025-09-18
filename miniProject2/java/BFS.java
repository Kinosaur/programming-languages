import java.util.*;

public class BFS {
    private int vertices; // Number of vertices
    private LinkedList<Integer>[] adjList; // Adjacency List

    public BFS(int v) {
        vertices = v;
        adjList = new LinkedList[v];
        for (int i = 0; i < v; ++i)
            adjList[i] = new LinkedList<>();
    }

    // Add edge to the graph
    public void addEdge(int v, int w) {
        adjList[v].add(w);
    }

    // Perform BFS traversal from a given source
    public void bfs(int start) {
        boolean[] visited = new boolean[vertices];
        Queue<Integer> queue = new LinkedList<>();

        visited[start] = true;
        queue.add(start);

        while (!queue.isEmpty()) {
            int node = queue.poll();
            System.out.print(node + " ");

            for (int neighbor : adjList[node]) {
                if (!visited[neighbor]) {
                    visited[neighbor] = true;
                    queue.add(neighbor);
                }
            }
        }
    }

    // Example usage
    public static void main(String[] args) {
        BFS graph = new BFS(5);
        graph.addEdge(0, 1);
        graph.addEdge(0, 2);
        graph.addEdge(1, 2);
        graph.addEdge(2, 0);
        graph.addEdge(2, 3);
        graph.addEdge(3, 3);
        graph.addEdge(3, 4);

        System.out.println("Breadth First Traversal starting from vertex 2:");
        graph.bfs(2);
    }
}