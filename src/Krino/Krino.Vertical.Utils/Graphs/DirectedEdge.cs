namespace Krino.Vertical.Utils.Graphs
{
    /// <summary>
    /// Directed edge.
    /// </summary>
    /// <typeparam name="V">type of the vertex</typeparam>
    /// <typeparam name="E">type of the edge</typeparam>
    public class DirectedEdge<V, E>
    {
        public DirectedEdge(V from, V to)
        {
            From = from;
            To = to;
        }

        public V From { get; private set; }

        public V To { get; private set; }

        public E Value { get; set; }
    }
}
