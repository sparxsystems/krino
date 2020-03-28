using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Vertical.Utils.Graphs
{
    public class Vertex<T>
    {
        public Vertex(string id)
        {
            Id = id;
        }

        public string Id { get; private set; }

        public T Value { get; set; }
    }
}
