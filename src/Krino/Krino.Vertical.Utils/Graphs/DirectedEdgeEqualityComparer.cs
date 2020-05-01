using System.Collections.Generic;

namespace Krino.Vertical.Utils.Graphs
{
    public class DirectedEdgeEqualityComparer<V> : IEqualityComparer<DirectedEdge<V>>
    {
        private IEqualityComparer<V> myEdgeValueComparer;

        public DirectedEdgeEqualityComparer(IEqualityComparer<V> edgeValueEqualityComparer = null)
        {
            myEdgeValueComparer = edgeValueEqualityComparer ?? EqualityComparer<V>.Default;
        }

        public bool Equals(DirectedEdge<V> x, DirectedEdge<V> y)
        {
            bool result = x.From == y.From &&
                          x.To == y.To &&
                          myEdgeValueComparer.Equals(x.Value, y.Value);
            return result;
        }

        public int GetHashCode(DirectedEdge<V> obj)
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ obj.From.GetHashCode();
            hash = (hash * 16777619) ^ obj.To.GetHashCode();
            hash = (hash * 16777619) ^ obj.Value.GetHashCode();

            return hash;
        }
    }
}
