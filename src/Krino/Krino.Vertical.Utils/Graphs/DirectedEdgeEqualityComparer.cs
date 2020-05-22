using System.Collections.Generic;

namespace Krino.Vertical.Utils.Graphs
{
    /// <summary>
    /// Defines the equality comparer for the directed edge of the graph.
    /// </summary>
    /// <typeparam name="V"></typeparam>
    /// <typeparam name="E"></typeparam>
    public class DirectedEdgeEqualityComparer<V, E> : IEqualityComparer<DirectedEdge<V, E>>
    {
        private IEqualityComparer<V> myVertexComparer;
        private IEqualityComparer<E> myEdgeComparer;

        public DirectedEdgeEqualityComparer(IEqualityComparer<V> vertexEqualityComparer, IEqualityComparer<E> edgeEqualityComparer = null)
        {
            myVertexComparer = vertexEqualityComparer ?? EqualityComparer<V>.Default;
            myEdgeComparer = edgeEqualityComparer ?? EqualityComparer<E>.Default;
        }

        public bool Equals(DirectedEdge<V, E> x, DirectedEdge<V, E> y)
        {
            bool result = myVertexComparer.Equals(x.From, y.From) &&
                          myVertexComparer.Equals(x.To, y.To) &&
                          myEdgeComparer.Equals(x.Value, y.Value);
            return result;
        }

        public int GetHashCode(DirectedEdge<V, E> obj)
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ obj.From.GetHashCode();
            hash = (hash * 16777619) ^ obj.To.GetHashCode();
            hash = (hash * 16777619) ^ obj.Value.GetHashCode();

            return hash;
        }
    }
}
