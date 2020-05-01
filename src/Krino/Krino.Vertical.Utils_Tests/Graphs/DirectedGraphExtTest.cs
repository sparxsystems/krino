using Krino.Vertical.Utils.Graphs;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Vertical.Utils_Tests.Graphs
{
    [TestFixture]
    public class DirectedGraphExtTest
    {
        [Test]
        public void FindAllPaths()
        {
            DirectedGraph<int, string> graph = new DirectedGraph<int, string>();
            graph.AddVertex("0", 0);
            graph.AddVertex("1", 1);
            graph.AddVertex("2", 2);
            graph.AddVertex("3", 3);
            graph.AddEdge("0", "2", "02");
            graph.AddEdge("0", "3", "03");
            graph.AddEdge("0", "1", "01");
            graph.AddEdge("2", "0", "20");
            graph.AddEdge("2", "0", "*20"); // there are two edges grom from the same to the same vertwx.
            graph.AddEdge("2", "1", "21");
            graph.AddEdge("1", "3", "13");

            IEnumerable<IReadOnlyList<DirectedEdge<string>>> result = graph.FindAllPaths("2", "3");

            List<string> allPaths = graph.FindAllPaths("2", "3").Select(x => string.Join("-", x.Select(y => y.Value))).ToList();
            Assert.AreEqual(5, allPaths.Count);
            Assert.IsTrue(allPaths.Contains("20-03"));
            Assert.IsTrue(allPaths.Contains("*20-03"));
            Assert.IsTrue(allPaths.Contains("20-01-13"));
            Assert.IsTrue(allPaths.Contains("*20-01-13"));
            Assert.IsTrue(allPaths.Contains("21-13"));

            allPaths = graph.FindAllPaths("2", "2").Select(x => string.Join("-", x.Select(y => y.Value))).ToList();
            Assert.AreEqual(2, allPaths.Count);
            Assert.IsTrue(allPaths.Contains("20-02"));
            Assert.IsTrue(allPaths.Contains("*20-02"));

            // There is no path from 3 to 2.
            allPaths = graph.FindAllPaths("3", "2").Select(x => string.Join("-", x.Select(y => y.Value))).ToList();
            Assert.AreEqual(0, allPaths.Count);

            // Vertex to itself.
            graph.AddEdge("2", "2", "22");
            allPaths = graph.FindAllPaths("2", "2").Select(x => string.Join("-", x.Select(y => y.Value))).ToList();
            Assert.AreEqual(3, allPaths.Count);
            Assert.IsTrue(allPaths.Contains("20-02"));
            Assert.IsTrue(allPaths.Contains("*20-02"));
            Assert.IsTrue(allPaths.Contains("22"));
        }
    }
}
