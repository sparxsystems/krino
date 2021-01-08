using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Diagnostic;
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
        /// Finds all possible paths via edges between two vertices.
        /// </summary>
        /// <typeparam name="V"></typeparam>
        /// <typeparam name="E"></typeparam>
        /// <param name="graph"></param>
        /// <param name="start"></param>
        /// <param name="destination"></param>
        /// <param name="vertexComparer"></param>
        /// <returns></returns>
        public static IEnumerable<IReadOnlyList<DirectedEdge<V, E>>> FindAllEdges<V, E>(this IDirectedGraph<V, E> graph,
            V start, V destination,
            IEqualityComparer<V> vertexComparer = null)
        {
            using (Trace.Entering())
            {
                vertexComparer = vertexComparer ?? EqualityComparer<V>.Default;

                List<List<DirectedEdge<V, E>>> edgePaths = new List<List<DirectedEdge<V, E>>>();

                var vertexPaths = graph.FindAllPaths(start, destination, vertexComparer).ToList();
                foreach (var path in vertexPaths)
                {
                    List<List<DirectedEdge<V, E>>> edgePossibilities = new List<List<DirectedEdge<V, E>>>();
                    for (int i = 1; i < path.Count; ++i)
                    {
                        var from = path[i - 1];
                        var to = path[i];

                        var fromToEdges = graph.GetEdges(from, to).ToList();
                        if (fromToEdges.Count > 0)
                        {
                            edgePossibilities.Add(fromToEdges);
                        }
                    }
                    var allPathVariations = edgePossibilities.GetVariations();
                    foreach (var pathVariation in allPathVariations)
                    {
                        edgePaths.Add(pathVariation.ToList());
                    }
                }

                return edgePaths;
            }
        }

        /// <summary>
        /// Find all possible paths between two vertices.
        /// </summary>
        /// <typeparam name="V"></typeparam>
        /// <typeparam name="E"></typeparam>
        /// <param name="graph"></param>
        /// <param name="start"></param>
        /// <param name="destination"></param>
        /// <param name="vertexComparer"></param>
        /// <returns></returns>
        public static IEnumerable<IReadOnlyList<V>> FindAllPaths<V, E>(this IDirectedGraph<V, E> graph, V start, V destination,
            IEqualityComparer<V> vertexComparer = null)
        {
            using (Trace.Entering())
            {
                vertexComparer = vertexComparer ?? EqualityComparer<V>.Default;

                HashSet<V> used = new HashSet<V>(vertexComparer);
                used.Add(start);

                List<V> localPath = new List<V>();

                IEnumerable<IReadOnlyList<V>> result = graph.FindAllPathsInternal(start, destination, localPath, used, vertexComparer);
                return result;
            }
        }

        private static IEnumerable<List<V>> FindAllPathsInternal<V, E>(this IDirectedGraph<V, E> graph,
            V start, V destination,
            List<V> localPath,
            HashSet<V> alreadyUsed,
            IEqualityComparer<V> vertexComparer)
        {
            using (Trace.Entering())
            {
                localPath.Add(start);

                var toGo = graph.GetEdgesGoingFrom(start)
                    .Select(x => x.To)
                    .ToHashSet(vertexComparer);

                foreach (var to in toGo)
                {
                    if (vertexComparer.Equals(to, destination))
                    {
                        localPath.Add(to);

                        // Provide the whole path.
                        List<V> result = localPath.ToList();

                        localPath.RemoveAt(localPath.Count - 1);

                        yield return result;

                        // If the self-reference is not included yet.
                        if (!vertexComparer.Equals(start, to))
                        {
                            List<V> pathWithSelfreferencingDestination = null;
                            bool selfReferencingDestination = graph.GetEdges(destination, destination).Any();
                            if (selfReferencingDestination)
                            {
                                pathWithSelfreferencingDestination = result.ToList();
                                pathWithSelfreferencingDestination.Add(destination);
                                yield return pathWithSelfreferencingDestination;
                            }

                            // If an edge to itself exists then include it to the result.
                            if (toGo.Contains(start))
                            {
                                var pathWithSelfReference = result.ToList();
                                pathWithSelfReference.Insert(result.Count - 1, start);
                                yield return pathWithSelfReference;

                                if (selfReferencingDestination)
                                {
                                    var pathWithBothSelfReferences = pathWithSelfReference.ToList();
                                    pathWithBothSelfReferences.Add(destination);
                                    yield return pathWithBothSelfReferences;
                                }
                            }
                        }
                    }
                    else if (alreadyUsed.Add(to))
                    {
                        IEnumerable<List<V>> foundPaths = graph.FindAllPathsInternal(to, destination, localPath, alreadyUsed, vertexComparer);
                        foreach (var path in foundPaths)
                        {
                            // Return the path.
                            yield return path;
                        }

                        alreadyUsed.Add(start);
                    }
                }

                localPath.RemoveAt(localPath.Count - 1);
            }
        }
    }
}
