using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Graphs
{
    /// <summary>
    /// Utility and extending functionality for DirectedGraph.
    /// </summary>
    public static class DirectedGraphExt
    {
        /// <summary>
        /// Adds sequence of vertices to the graph.
        /// </summary>
        /// <typeparam name="V"></typeparam>
        /// <typeparam name="E"></typeparam>
        /// <param name="graph"></param>
        /// <param name="vertices"></param>
        public static void AddVertices<V, E>(this IDirectedGraph<V, E> graph, IEnumerable<V> vertices)
        {
            foreach (V vertex in vertices)
            {
                graph.AddVertex(vertex);
            }
        }

        /// <summary>
        /// Finds all possible paths between two vertices using the DFS algorithm.
        /// </summary>
        /// <typeparam name="V"></typeparam>
        /// <typeparam name="E"></typeparam>
        /// <param name="graph"></param>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <returns></returns>
        public static IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> FindAllPaths<V, E>(this IDirectedGraph<V, E> graph, V from, V to,
            IEqualityComparer<V> vertexComparer = null)
        {
            vertexComparer = vertexComparer ?? EqualityComparer<V>.Default;

            HashSet<V> visited = new HashSet<V>(vertexComparer);
            List<DirectedEdge<V, E>> localPath = new List<DirectedEdge<V, E>>();

            IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> result = graph.FindAllPathsInternal(from, to, localPath, visited, vertexComparer);
            return result;
        }


        private static IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> FindAllPathsInternal<V, E>(this IDirectedGraph<V, E> graph, V currentVertex, V to,
            List<DirectedEdge<V, E>> localPath,
            HashSet<V> visited,
            IEqualityComparer<V> vertexComparer)
        {
            visited.Add(currentVertex);

            IEnumerable<DirectedEdge<V, E>> edgesGoingFrom = graph.GetEdgesGoingFrom(currentVertex);
            foreach (DirectedEdge<V, E> edge in edgesGoingFrom)
            {
                if (vertexComparer.Equals(edge.To, to) || !visited.Contains(edge.To))
                {
                    if (vertexComparer.Equals(edge.To, to))
                    {
                        localPath.Add(edge);
                        
                        yield return localPath.ToList();
                        
                        localPath.RemoveAt(localPath.Count - 1);
                    }
                    else if (!vertexComparer.Equals(edge.From, edge.To))
                    {
                        localPath.Add(edge);

                        IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> foundPaths = graph.FindAllPathsInternal(edge.To, to, localPath, visited, vertexComparer);
                        foreach (IReadOnlyList<DirectedEdge<V, E>> path in foundPaths)
                        {
                            yield return path;
                        }
                        
                        localPath.RemoveAt(localPath.Count - 1);
                    }
                }
            }

            visited.Remove(currentVertex);
        }
    }
}
