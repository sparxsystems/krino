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
        /// Finds all possible paths between two vertices using the DFS algorithm.
        /// </summary>
        /// <typeparam name="V"></typeparam>
        /// <typeparam name="E"></typeparam>
        /// <param name="graph"></param>
        /// <param name="fromId"></param>
        /// <param name="toId"></param>
        /// <returns></returns>
        public static IEnumerable<IReadOnlyList<DirectedEdge<E>>> FindAllPaths<V, E>(this IDirectedGraph<V, E> graph, string fromId, string toId)
        {
            HashSet<string> visited = new HashSet<string>();
            List<DirectedEdge<E>> localPath = new List<DirectedEdge<E>>();

            IEnumerable<IReadOnlyList<DirectedEdge<E>>> result = graph.FindAllPathsInternal(fromId, toId, localPath, visited);
            return result;
        }


        private static IEnumerable<IReadOnlyList<DirectedEdge<E>>> FindAllPathsInternal<V, E>(this IDirectedGraph<V, E> graph, string currentVertexId, string toId,
            List<DirectedEdge<E>> localPath,
            HashSet<string> visited)
        {
            visited.Add(currentVertexId);

            IEnumerable<DirectedEdge<E>> edgesGoingFrom = graph.GetEdgesGoingFrom(currentVertexId);
            foreach (DirectedEdge<E> edge in edgesGoingFrom)
            {
                if (edge.To == toId || !visited.Contains(edge.To))
                {
                    if (edge.To == toId)
                    {
                        localPath.Add(edge);
                        
                        yield return localPath;
                        
                        localPath.RemoveAt(localPath.Count - 1);
                    }
                    else if (edge.From != edge.To)
                    {
                        localPath.Add(edge);

                        IEnumerable<IReadOnlyList<DirectedEdge<E>>> foundPaths = graph.FindAllPathsInternal(edge.To, toId, localPath, visited);
                        foreach (IReadOnlyList<DirectedEdge<E>> path in foundPaths)
                        {
                            yield return path;
                        }
                        
                        localPath.RemoveAt(localPath.Count - 1);
                    }

                    
                }
            }

            visited.Remove(currentVertexId);
        }
    }
}
