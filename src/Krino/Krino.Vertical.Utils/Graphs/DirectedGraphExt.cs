using Krino.Vertical.Utils.Diagnostic;
using System;
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
            using (Trace.Entering())
            {
                foreach (V vertex in vertices)
                {
                    graph.AddVertex(vertex);
                }
            }
        }

        /// <summary>
        /// Finds all possible edge sequeces between two vertices.
        /// </summary>
        /// <typeparam name="V"></typeparam>
        /// <typeparam name="E"></typeparam>
        /// <param name="graph"></param>
        /// <param name="from"></param>
        /// <param name="to"></param>
        /// <param name="predicate"></param>
        /// <param name="vertexComparer"></param>
        /// <param name="edgeValueComparer"></param>
        /// <returns></returns>
        public static IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> FindAllEdges<V, E>(this IDirectedGraph<V, E> graph, V from, V to,
            Predicate<List<DirectedEdge<V, E>>> predicate = null,
            IEqualityComparer<V> vertexComparer = null,
            IEqualityComparer<E> edgeValueComparer = null)
        {
            using (Trace.Entering())
            {
                vertexComparer = vertexComparer ?? EqualityComparer<V>.Default;
                edgeValueComparer = edgeValueComparer ?? EqualityComparer<E>.Default;

                HashSet<DirectedEdge<V, E>> visited = new HashSet<DirectedEdge<V, E>>(new DirectedEdgeEqualityComparer<V, E>(vertexComparer, edgeValueComparer));
                List<DirectedEdge<V, E>> localPath = new List<DirectedEdge<V, E>>();

                IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> result = graph.FindAllEdgesInternal(from, to, localPath, visited, vertexComparer, predicate);
                return result;
            }
        }


        private static IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> FindAllEdgesInternal<V, E>(this IDirectedGraph<V, E> graph, V currentVertex, V to,
            List<DirectedEdge<V, E>> localPath,
            HashSet<DirectedEdge<V, E>> alreadyUsedEdges,
            IEqualityComparer<V> vertexComparer,
            Predicate<List<DirectedEdge<V, E>>> predicate)
        {
            using (Trace.Entering())
            {
                IEnumerable<DirectedEdge<V, E>> edgesGoingFrom = graph.GetEdgesGoingFrom(currentVertex);
                foreach (DirectedEdge<V, E> edge in edgesGoingFrom)
                {
                    if (vertexComparer.Equals(edge.To, to))
                    {
                        localPath.Add(edge);

                        if (predicate == null || predicate(localPath))
                        {
                            yield return localPath.ToList();
                        }

                        localPath.RemoveAt(localPath.Count - 1);
                    }
                    else if (alreadyUsedEdges.Add(edge))
                    {
                        localPath.Add(edge);

                        if (predicate == null || predicate(localPath))
                        {
                            IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> foundPaths = graph.FindAllEdgesInternal(edge.To, to, localPath, alreadyUsedEdges, vertexComparer, predicate);
                            foreach (IReadOnlyList<DirectedEdge<V, E>> path in foundPaths)
                            {
                                yield return path;
                            }
                        }

                        localPath.RemoveAt(localPath.Count - 1);
                        alreadyUsedEdges.Remove(edge);
                    }
                }
            }
        }
    }
}
