using System.Collections.Generic;

namespace Krino.Vertical.Utils.Graphs
{
    public class EdgeEqualityComparer<V> : IEqualityComparer<Edge<V>>
    {
        private IEqualityComparer<V> myEdgeValueComparer;

        public EdgeEqualityComparer(IEqualityComparer<V> edgeValueEqualityComparer = null)
        {
            myEdgeValueComparer = edgeValueEqualityComparer ?? EqualityComparer<V>.Default;
        }

        public bool Equals(Edge<V> x, Edge<V> y)
        {
            bool result = x.From == y.From &&
                          x.To == y.To &&
                          x.Type == y.Type &&
                          myEdgeValueComparer.Equals(x.Value, y.Value);
            return result;
        }

        public int GetHashCode(Edge<V> obj)
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ obj.From.GetHashCode();
            hash = (hash * 16777619) ^ obj.To.GetHashCode();
            hash = (hash * 16777619) ^ obj.Type.GetHashCode();
            hash = (hash * 16777619) ^ obj.Value.GetHashCode();

            return hash;
        }
    }
}
