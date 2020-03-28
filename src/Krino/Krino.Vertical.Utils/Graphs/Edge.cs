namespace Krino.Vertical.Utils.Graphs
{
    public class Edge<T>
    {
        public string From { get; set; }

        public string To { get; set; }

        public string Type { get; set; }

        public T Value { get; set; }
    }
}
