namespace Krino.Vertical.Utils.Graphs
{
    /// <summary>
    /// Vertex of the graph.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class Vertex<T>
    {
        public Vertex(string id)
        {
            Id = id;
        }

        /// <summary>
        /// Unique identifier of the vertex.
        /// </summary>
        public string Id { get; private set; }

        public T Value { get; set; }
    }
}
