## **A Comparative Analysis of Graph Search Algorithms Across Programming Paradigms**

**Project Report**

### **Introduction**

Graph traversal algorithms are fundamental to computer science, providing the building blocks for solving a vast array of problems, from network routing to artificial intelligence. The two most foundational traversal methods, Breadth-First Search (BFS) and Depth-First Search (DFS), offer distinct strategies for exploring a graph's nodes and edges. While the algorithms themselves are universal, their implementation can vary dramatically depending on the programming paradigm used.

This report presents a detailed comparative analysis of BFS and DFS implemented in three distinct paradigms: **Imperative/Object-Oriented (Java)**, **Functional (Haskell)**, and **Logic (Prolog)**. By implementing the same set of algorithms to solve identical problems under a strict specification, we can explore the core strengths, weaknesses, and unique characteristics of each paradigm. We will analyze the expressiveness of each language, the semantic and abstract concepts that influenced the design, and conclude with a reflection on which paradigm is best suited for this class of problem in different contexts.

The analysis is based on a common specification: for a given directed or undirected graph, find a path from a source to a destination node. All implementations adhere to deterministic rules, including processing neighbors in ascending numerical order, ensuring that the results are comparable and correct.

-----

### **1. Source Code Implementations**

This section presents the complete, commented source code for each paradigm. The comments are designed to highlight how key algorithmic steps are realized within the language and how they adhere to the project's specifications.

#### **1.1 Imperative / Object-Oriented (Java)**

The Java code embodies the imperative paradigm by using mutable data structures, explicit loops, and sequential commands to describe the search process.

**Adjacency List Construction (from `java/UndirectedBFS.java`)**

  * **Strength:** The use of `TreeSet` is a clever and direct way to enforce the specification's requirement for sorted, unique neighbors. It offloads the policy of ordering and deduplication to a standard library data structure before the search begins.
  * **Weakness:** This approach requires an intermediate data structure (`TreeSet[]`) before converting to the final `int[][]` array, which may have a performance overhead for very large graphs compared to a more direct construction method.

<!-- end list -->

```java
// Adjacency lists are built using a TreeSet to automatically handle
// the spec's requirement for deduplicated and sorted neighbors.
@SuppressWarnings("unchecked")
TreeSet<Integer>[] neigh = new TreeSet[N + 1];
for (int i = 1; i <= N; i++) neigh[i] = new TreeSet<>();

for (int i = 0; i < M; i++) {
    int u = in.nextInt();
    int v = in.nextInt();
    if (1 <= u && u <= N) neigh[u].add(v);
    // For undirected graphs, the reverse edge is explicitly added.
    if (1 <= v && v <= N) neigh[v].add(u);
}

// The TreeSets are converted to primitive int arrays for efficient iteration during the search.
int[][] adj = new int[N + 1][];
for (int i = 1; i <= N; i++) {
    adj[i] = neigh[i].stream().distinct().sorted().mapToInt(Integer::intValue).toArray();
}
```

**BFS (`java/UndirectedBFS.java`)**

  * **Strength:** This code is a textbook implementation of BFS. Its logic is clear, direct, and easy for any programmer to follow. The use of `ArrayDeque` is efficient for a queue.
  * **Weakness:** The code is tightly coupled to its mutable state (`vis`, `parent`). This can make it harder to test in isolation or reuse in a concurrent setting compared to a functional approach.

<!-- end list -->

```java
// Main BFS logic using a queue and mutable arrays for visited and parent pointers.
private static String bfsPath(int N, int[][] adj, int SRC, int DST) {
    // A mutable boolean array tracks visited nodes.
    boolean[] vis = new boolean[N + 1];
    // A mutable int array stores the parent of each node for path reconstruction.
    int[] parent = new int[N + 1];
    Arrays.fill(parent, -1);

    // ArrayDeque provides an efficient O(1) queue.
    ArrayDeque<Integer> q = new ArrayDeque<>();
    q.add(SRC);
    vis[SRC] = true; // SPEC: Mark visited on enqueue.

    // A standard while loop continues as long as there are nodes to visit.
    while (!q.isEmpty()) {
        int u = q.remove();
        // SPEC: Terminate when the target node is dequeued.
        if (u == DST) return reconstruct(parent, SRC, DST);

        // Iterate through neighbors in the pre-sorted adjacency list.
        for (int v : adj[u]) {
            if (1 <= v && v <= N && !vis[v]) {
                vis[v] = true;      // Mutate visited array.
                parent[v] = u;    // Mutate parent array.
                q.add(v);         // Enqueue neighbor.
            }
        }
    }
    return "No path";
}
```

**DFS (`java/DirectedDFS.java`)**

  * **Strength:** The recursive implementation is elegant and concise. It naturally models the "go deep, then backtrack" nature of DFS, with the call stack managing the frontier implicitly.
  * **Weakness:** Relies on the call stack, which can lead to a `StackOverflowError` for graphs with very long paths. The path itself is also passed as a mutable `ArrayList`, which can be prone to errors if not handled carefully.

<!-- end list -->

```java
// DFS logic using recursion, which implicitly uses the call stack.
private static boolean dfs(int u, int DST, int[][] adj, boolean[] vis, ArrayList<Integer> path) {
    // SPEC: Mark visited on entry (preorder).
    vis[u] = true;
    path.add(u); // Add the current node to the mutable path list.

    if (u == DST) return true; // Base case: destination found.

    // Recurse on unvisited neighbors in the specified sorted order.
    for (int v : adj[u]) {
        if (1 <= v && v < vis.length && !vis[v]) {
            // If a recursive call finds the path, propagate success immediately.
            if (dfs(v, DST, adj, vis, path)) return true;
        }
    }

    // This is the backtracking step: if no path was found from u, remove it from the path.
    path.remove(path.size() - 1);
    return false;
}
```

-----

#### **1.2 Functional (Haskell)**

The Haskell code embodies the functional paradigm by using immutable data structures and pure functions, defining the search as a state transformation. A key feature of this implementation is the dual-builder strategy, which separates the choice of data structure from the search algorithm itself.

**Adjacency List Construction**

  * **Strength:** The separation of concerns is total. The search algorithms (`GraphBFS/BFS.hs`, `GraphDFS/DFS.hs`) are completely generic and unaware of the underlying graph representation. They only depend on a `succOf` function. This allows the developer to transparently swap out different builder strategies for performance or generality.
  * **Weakness:** The generic `Map`/`Set` builder (`GraphDFS/Directed.hs`, `GraphDFS/Undirected.hs`) is slightly less performant for integer-only graphs than the specialized `IntMap`/`IntSet` version (`GraphBFS/DirectedBFS.hs`, `GraphBFS/UndirectedBFS.hs`).

**Generic Builder (`GraphDFS/Undirected.hs`)**

```haskell
-- Uses generic, polymorphic Map and Set. Good for any node type that is Ord.
buildAdjacency :: [(Int, Int)] -> Adj
buildAdjacency es =
  let add u v m = Map.insertWith (++) u [v] m
      -- A fold builds the map, explicitly adding reverse edges.
      m1 = foldr (\(u, v) m -> add u v (add v u m)) Map.empty es
  -- The final map's lists are sorted and deduplicated.
  in Map.map (sort . Set.toList . Set.fromList) m1
```

**Specialized Builder (`GraphBFS/UndirectedBFS.hs`)**

```haskell
-- Uses IntMap and IntSet, which are highly optimized for Int keys.
buildAdjacency :: [(Int, Int)] -> Adj
buildAdjacency es =
  let step m (u,v) =
        -- A union operation on IntSets is efficient for collecting neighbors.
        let m1 = IM.insertWith IS.union u (IS.singleton v) m
            m2 = IM.insertWith IS.union v (IS.singleton u) m1
        in m2
      -- A strict foldl' prevents space leaks during construction.
      msets = foldl' step IM.empty es :: IM.IntMap IS.IntSet
  -- IntSet automatically handles sorting and deduplication.
  in IM.map IS.toList msets
```

**BFS (`GraphBFS/BFS.hs`)**

  * **Strength:** This function is a pure transformation. It has no side effects, and its behavior is completely determined by its inputs, making it easy to test and reason about. The flow of state (visited, parents, queue) is made explicit in the arguments of the `loop` function.
  * **Weakness:** The syntax can be challenging for those unfamiliar with functional programming. The concept of passing around and creating new state at every step is less direct than mutation.

<!-- end list -->

```haskell
-- BFS is a recursive loop that takes the current state (visited, parents, queue)
-- and returns the result based on the next state.
bfsPath :: (Int -> [Int]) -> Int -> Int -> Maybe [Int]
bfsPath succF s t
  | s == t    = Just [s]
  | otherwise = loop initialVisited initialParents (Seq.singleton s)
  where
    initialVisited = IS.singleton s -- An immutable set of visited nodes.
    initialParents = IM.empty       -- An immutable map for parent pointers.

    loop :: IS.IntSet -> IM.IntMap Int -> Seq Int -> Maybe [Int]
    loop visited parents queue =
      case Seq.viewl queue of
        EmptyL    -> Nothing -- Base case: queue is empty, no path.
        u :< rest ->
          -- SPEC: Terminate when the target node is dequeued.
          if u == t
            then Just (reconstruct parents s t)
            else
              -- Process neighbors to build the *new*, immutable state.
              let (visited', parents', rest') =
                    foldl' (step u) (visited, parents, rest) (succF u)
              in loop visited' parents' rest' -- Recurse with the new state.

    -- This function defines the state transition for one neighbor.
    -- It creates new data structures, fulfilling the "mark visited on enqueue" rule.
    step u (vi, pa, q) v
      | IS.member v vi = (vi, pa, q) -- If visited, the state is unchanged.
      | otherwise      = (IS.insert v vi, IM.insert v u pa, q |> v)
```

**DFS (`GraphDFS/DFS.hs`)**

  * **Strength:** By using an explicit frontier (a list acting as a stack), this implementation avoids the potential for stack overflow errors, making it more robust than a naive recursive solution. It clearly separates the concerns of the `seen` set from the `frontier` list.
  * **Weakness:** The path is accumulated with `path ++ [v]`, which is inefficient for lists (O(L) for a path of length L). While clear and readable, a production version might use a different structure (like a `Seq` or difference list) for O(1) appends.

<!-- end list -->

```haskell
-- DFS implemented with an explicit frontier (a list acting as a stack)
-- to manage state without deep recursion on the call stack.
-- The frontier holds tuples of (node_to_visit, path_so_far).
dfsPath :: (Ord a) => (a -> [a]) -> a -> a -> Maybe [a]
dfsPath succF s t = go Set.empty [(s, [s])] -- Initial state: empty visited set, frontier with start node.
  where
    go _ [] = Nothing -- Base case: frontier is empty, no path.
    go seen ((x, path) : rest)
      | x == t = Just path -- Base case: the current node is the target.
      | x `Set.member` seen = go seen rest -- Node already visited, continue.
      | otherwise =
          -- SPEC: Mark visited on entry (preorder).
          let seen' = Set.insert x seen -- Create a new immutable visited set.
              -- Get neighbors that have not been seen yet.
              nbrs = filter (`Set.notMember` seen') (succF x)
              -- Create new items for the frontier.
              next = [(v, path ++ [v]) | v <- nbrs]
              -- Prepend new items to achieve depth-first behavior.
              frontier = next ++ rest
          in go seen' frontier -- Recurse with the new state.
```

-----

#### **1.3 Logic (Prolog)**

The Prolog code defines the search as a set of logical rules and facts. The execution is driven by Prolog's unification and backtracking engine.

**Adjacency List Construction (`prolog/dfs_num.pl`)**

  * **Strength:** The construction process is declarative. Predicates like `normalize_edges` and `sort_neighbors` clearly define transformations on the edge list.
  * **Weakness:** The `add_edge` predicate uses `select/3`, which has linear time complexity. For very large graphs, this could be less efficient than the tree-based maps used in Haskell and Java.

<!-- end list -->

```prolog
% A list of all nodes from 1..N is created first.
build_adjacency(N, Edges, Adj) :-
    numlist(1, N, Ns),
    maplist(init_node, Ns, Init),
    % foldl applies the add_edge rule to each edge to build the final adjacency list.
    foldl(add_edge, Edges, Init, Adj1),
    true, Adj = Adj1.

init_node(U, U-[]).

add_edge(U-V, In, Out) :-
    % This finds and replaces the neighbor list for node U.
    select(U-Ns, In, Rest), !,
    Out = [U-[V|Ns] | Rest].
add_edge(_, In, In). % If U is not in the list, do nothing.

% sort/2 is used to deduplicate and sort neighbors numerically, as per the spec.
sort_neighbors(Adj0, Adj) :- maplist(sort_one, Adj0, Adj).
sort_one(U-Vs0, U-Vs) :- sort(Vs0, Vs).
```

**BFS (`prolog/bfs_num.pl`)**

  * **Strength:** The logic is broken down into small, readable rules. The pattern matching on `[U|Qs]` to dequeue an item is clear and idiomatic.
  * **Weakness:** The queue is simulated with a list, and `append/3` is used for enqueuing. This is inefficient (O(N) for a list of length N) and is a classic pedagogical trade-off in Prolog.

<!-- end list -->

```prolog
% BFS entry point: sets up initial state and calls the queue processing rule.
bfs(Adj, Src, Dst, Path) :-
    empty_assoc(Visited0),
    put_assoc(Src, Visited0, true, Visited), % Initial visited map (associative list).
    empty_assoc(Parent0),                   % Initial parent map.
    % Begin the search with a queue containing only the source.
    bfs_q(Adj, [Src], Visited, Parent0, Dst, ParentOut, Found),
    Found == true,
    reconstruct(ParentOut, Src, Dst, Path).

% Rule for a non-empty queue.
bfs_q(Adj, [U|Qs], Vis, Par, Dst, ParOut, Found) :-
    ( U =:= Dst -> % SPEC: Terminate when the target node is dequeued.
        ParOut = Par, Found = true
    ; % Otherwise, enqueue unvisited neighbors and recurse.
      neighbors(Adj, U, Ns),
      % This predicate updates Visited/Parent maps, fulfilling "mark on enqueue".
      enqueue_new(Ns, U, Qs, Vis, Vis1, Par, Par1, Qs1),
      bfs_q(Adj, Qs1, Vis1, Par1, Dst, ParOut, Found)
    ).
```

**DFS (`prolog/dfs_num.pl`)**

  * **Strength:** This is the most elegant and concise of all DFS implementations. It reads less like a program and more like a logical definition of a path. The language's built-in backtracking engine handles all the complexity of exploring different branches.
  * **Weakness:** The heavy use of cuts (`!`) makes the logic rigid; it is designed to find only the *first* path and immediately stop. Modifying it to find all paths would require removing the cuts and could change the performance profile. The `member/2` check for visited nodes is O(N), which is inefficient for large paths.

<!-- end list -->

```prolog
% DFS entry point: calls the recursive visit rule and reverses the result.
dfs(Adj, Src, Dst, Path) :-
    % dfs_visit finds a path; the cut (!) commits to the very first solution found.
    dfs_visit(Adj, Src, Dst, [Src], Rev), !,
    reverse(Rev, Path).

% Base case: If the current node is the destination, the accumulated path is a valid solution.
dfs_visit(_Adj, Dst, Dst, Acc, Acc) :- !.
% Recursive rule for visiting a node.
dfs_visit(Adj, U, Dst, Vis, Path) :-
    neighbors(Adj, U, Ns),      % Get neighbors of U (already sorted).
    member(V, Ns),              % Choose a neighbor V. Prolog's engine will backtrack and try all.
    \+ member(V, Vis),          % Succeeds if V is NOT in the Visited list.
    % SPEC: Mark on entry is achieved by adding V to the head of the visited list
    % before the recursive call. The cut commits to the first path found.
    dfs_visit(Adj, V, Dst, [V|Vis], Path), !.
```

-----

### **2. Comparison of Expressiveness**

All three paradigms successfully solved the problem, but they offered vastly different development experiences in terms of difficulty, code size, and how naturally they expressed the problem's domain.

#### **2.1 Implementation Difficulty**

  * **Imperative (Java):** This approach was the most straightforward and required the least abstract thinking. The logic is a literal, step-by-step implementation of the algorithms as they are often taught, making it the **easiest to write** for developers familiar with conventional C-style languages. The main complexity lies in boilerplate code for I/O and class structure.

  * **Functional (Haskell):** The primary challenge was conceptual—shifting from thinking about a sequence of actions to a series of **data transformations**. Managing the flow of immutable state requires more upfront design but results in code that is highly modular and predictable. The dual-builder strategy, for example, is a powerful feature that requires a good grasp of the type system.

  * **Logic (Prolog):** The difficulty varied dramatically. **DFS was trivial to implement**, as it perfectly matches Prolog's native depth-first search and backtracking engine. It requires almost no procedural thought. In contrast, **BFS was significantly harder**, requiring the manual simulation of a FIFO queue and explicit state passing, which feels less natural in the language.

#### **2.2 Code Volume & Conciseness**

| Paradigm | Core Algorithm LOC (Approx.) | Total File LOC (Approx.) | Conciseness |
| :--- | :--- | :--- | :--- |
| **Java** | 30-40 lines | 90-110 lines | Low (Verbose) |
| **Haskell** | 20-25 lines | 60-80 lines | High (Dense) |
| **Prolog** | 10-20 lines (DFS), 25-35 (BFS) | 70-90 lines | Very High (for DFS) |

**Java** was by far the most verbose, primarily due to boilerplate for class definitions, I/O handling, and explicit type declarations. The core logic is compact, but the surrounding scaffolding is significant.

**Haskell** was the most consistently concise across both algorithms. Higher-order functions (`foldl'`, `map`), powerful type inference, and pattern matching allow for very dense and expressive code where single lines often accomplish what would require a full loop in Java.

**Prolog** was the undisputed champion of conciseness for **DFS**, which is reduced to just a few logical rules. Its BFS implementation, however, was comparable in length to Haskell's due to the need for manual queue and state management logic.

#### **2.3 Natural Fit for the Problem**

  * **Imperative (Java):** Feels like a **very natural fit for both algorithms**. The mental model of "traversing" a graph by changing state over time is perfectly aligned with the paradigm. It describes the *process* of searching.

  * **Functional (Haskell):** Also a very natural fit, but from a different perspective. It excels at defining the search as a **pure, mathematical transformation** from a graph to a path. The clean separation between data construction (the builders) and the pure search function is an extremely natural and powerful pattern in Haskell. It describes the *properties* of the result.

  * **Logic (Prolog):** A **perfect, native fit for DFS**. The code reads like a logical specification of what a path is, and Prolog's engine performs the search automatically. However, it is an **unnatural fit for BFS**, which requires forcing a specific, breadth-first traversal order onto an engine that is fundamentally depth-first. This involves "fighting" the language's natural tendencies.

-----

### **3. Semantics & Abstraction**

The core principles of each paradigm fundamentally shaped the design, behavior, and abstractions present in the code.

#### **3.1 State Management: A Core Divide**

The most significant conceptual difference across the paradigms was the handling of state (e.g., the `visited` set).

  * **In Java (Mutation):** A single `boolean[] visited` array is created on the heap and is **mutated** in-place. This is highly memory-efficient for the task but introduces the risk of bugs if multiple parts of a program were to interact with the same graph search instance. The core concept is a **shared, mutable state** that is modified by different parts of the program over time.

  * **In Haskell (Transformation):** State is **immutable**. When a node is visited, functions like `Set.insert` don't change the original set; they return a *new* set that includes the new element. This is achieved efficiently via **persistent data structures**, which share memory between old and new versions. This approach eliminates side effects, making the code easier to reason about and test in isolation. The core concept is a **stateless transformation**, where functions are pure and their output depends only on their input.

  * **In Prolog (Threading):** State is "threaded" through predicate arguments. In `bfs_q`, the `Vis` map is an input, and a new `Vis1` map is generated and used as an input to the recursive call. This is conceptually similar to Haskell's approach but managed by the logic engine's unification process.

#### **3.2 Control Flow: Loops vs. Recursion vs. Backtracking**

  * **Java** uses explicit, imperative control flow structures. The `while` loop in BFS and the recursive calls in DFS are standard mechanisms for managing iteration and program flow. Control is fine-grained and directed by the programmer at every step, including the use of `return` to terminate early.

  * **Haskell** relies on **recursion** as its primary control flow mechanism. The `loop` function in BFS and the `go` function in DFS are recursive definitions that describe the entire computation. There is no concept of "early return"; instead, the logic is structured to evaluate to the final result, which is then passed up the call chain.

  * **Prolog's DFS** is driven by its most powerful and unique feature: **backtracking**. The `member(V, Ns)` predicate doesn't just check for membership; it's a *generator* that successively binds `V` to each neighbor. If a subsequent rule in the clause fails, Prolog automatically backtracks and tries the next binding. The **cut (`!`)** is a powerful, non-local control structure that prunes the search space, committing the engine to the first successful path it finds and preventing it from looking for alternatives.

#### **3.3 Abstraction: Methods vs. Higher-Order Functions vs. Rules**

  * **In Java (Object-Oriented):** Abstraction is achieved through classes and methods. The `bfsPath` and `dfsPath` methods encapsulate the logic, but they are tightly bound to the specific data representation they receive (`int[][]`). Changing the graph representation would require changing these methods.

  * **In Haskell (Functional):** Abstraction is achieved through **higher-order functions**. The `bfsPath` and `dfsPath` algorithms are completely decoupled from the graph's physical structure. They simply require a successor function (`succOf`) as an argument. This is an incredibly powerful form of dependency injection that allows the same search code to work with wildly different underlying data structures (e.g., `Map` vs. `IntMap`), as demonstrated by the dual-builder strategy.

  * **In Prolog (Logic):** Abstraction is achieved through **rules and relations**. The `dfs_visit` predicate defines an abstract relationship between a start node, an end node, and a path. It doesn't describe how to compute it, only the logical conditions that must be true for it to exist. The "computation" is performed by the Prolog engine's search for a proof.

-----

### **4. Reflection & Recommendation**

#### **4.1 Intuitiveness of Each Paradigm**

The intuitiveness of a paradigm is highly dependent on a developer's background and mental model of programming.

  * The **Imperative (Java)** approach is often the most intuitive for the widest audience of programmers. Its step-by-step, state-modifying nature is a direct and familiar way to express algorithms.
  * The **Logic (Prolog)** approach for **DFS is uniquely elegant and intuitive** once the concept of backtracking is understood. It allows the programmer to simply state the problem's definition and let the engine find the solution.
  * The **Functional (Haskell)** approach becomes deeply intuitive after an initial learning curve. Once the concept of data transformation clicks, the ability to compose pure, predictable functions leads to a powerful and robust way of reasoning about complex logic, free from the worries of side effects.

#### **4.2 Real-World Project Recommendations**

The best choice of paradigm is a trade-off between the nature of the problem, the required performance characteristics, and the priorities of the project and team. 🎯

  * **Imperative / Object-Oriented (Java):**

      * **Recommend for:** General-purpose applications, large-scale enterprise systems, and Android development.
      * **Why:** Its performance is excellent, the ecosystem of libraries and tools (e.g., for web servers, databases, UI) is vast, and the developer talent pool is the largest. It is a robust, predictable, and industry-standard choice for building complex, stateful applications.

  * **Functional (Haskell):**

      * **Recommend for:** Implementing mission-critical, complex algorithms where **correctness, reliability, and maintainability are paramount**.
      * **Why:** The strong type system and guarantees of purity and immutability can prevent entire classes of subtle bugs (e.g., race conditions, unexpected state modifications). It is an outstanding choice for the core logic of compilers, financial modeling tools, data analysis pipelines, and network protocols where a bug could have severe consequences.

  * **Logic (Prolog):**

      * **Recommend for:** **Specialized problem domains** that are naturally expressed as a set of rules, constraints, or symbolic reasoning.
      * **Why:** It excels in areas like **Artificial Intelligence (e.g., expert systems), natural language processing, schedulers, and theorem provers**. For problems that require a built-in search and pattern-matching engine, Prolog can provide a massive productivity advantage over implementing such an engine from scratch in another language. It is less suited for general-purpose application development.