namespace Krino.Vertical.Utils.Graphs
{
    /// <summary>
    /// Directed edge.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class DirectedEdge<T>
    {
        public DirectedEdge(string from, string to)
        {
            From = from;
            To = to;
        }

        public string From { get; private set; }

        public string To { get; private set; }

        public T Value { get; set; }
    }
}
