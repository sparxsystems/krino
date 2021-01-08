using Krino.Vertical.Utils.Graphs;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.Graphs
{
    [TestFixture]
    public class DirectedGraphExtTest
    {
        [Test]
        public void FindAllEdges()
        {
            DirectedGraph<int, string> graph = new DirectedGraph<int, string>();
            graph.AddVertex(0);
            graph.AddVertex(1);
            graph.AddVertex(2);
            graph.AddVertex(3);
            graph.AddEdge(0, 1, "01");
            graph.AddEdge(1, 2, "12");
            graph.AddEdge(0, 2, "02");
            graph.AddEdge(0, 3, "03");

            List<string> allPaths = graph.FindAllEdges(0, 2).Select(x => string.Join("-", x.Select(y => y.Value))).ToList();
            Assert.AreEqual(2, allPaths.Count);
            Assert.AreEqual("01-12", allPaths[0]);
            Assert.AreEqual("02", allPaths[1]);
        }

        [Test]
        public void FindAllEdges_ViaItself_MiddleStep()
        {
            DirectedGraph<string, string> graph = new DirectedGraph<string, string>();
            graph.AddVertex("A");
            graph.AddVertex("O");
            graph.AddVertex("I");
            graph.AddEdge("I", "O", "IO");
            graph.AddEdge("O", "O", "OO"); // to itself
            graph.AddEdge("O", "A", "OA");

            // Get all paths from I to A.
            List<IReadOnlyList<DirectedEdge<string, string>>> result = graph.FindAllEdges("I", "A").ToList();
            List<string> allPaths = graph.FindAllEdges("I", "A").Select(x => string.Join("-", x.Select(y => y.Value))).ToList();

            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("IO-OA", allPaths[0]);
            Assert.AreEqual("IO-OO-OA", allPaths[1]);
            
        }

        [Test]
        public void FindAllEdges_ToItself()
        {
            DirectedGraph<int, string> graph = new DirectedGraph<int, string>();
            graph.AddVertex(0);
            graph.AddEdge(0, 0, ""); // goes to itself

            // Get all paths from A to A.
            List<IReadOnlyList<DirectedEdge<int, string>>> result = graph.FindAllEdges(0, 0).ToList();

            Assert.AreEqual(1, result.Count);
            Assert.AreEqual(1, result[0].Count);
            Assert.AreEqual(0, result[0][0].From);
            Assert.AreEqual(0, result[0][0].To);
        }

        [Test]
        public void FindAllEdges_ToItself_Indirectly()
        {
            DirectedGraph<string, string> graph = new DirectedGraph<string, string>();
            graph.AddVertex("A");
            graph.AddVertex("O");
            graph.AddEdge("A", "O", "Pattern1");
            graph.AddEdge("O", "A", "Pattern1");
            graph.AddEdge("O", "A", "Pattern2");

            // Get all paths from O to O.
            List<IReadOnlyList<DirectedEdge<string, string>>> result = graph.FindAllEdges("O", "O").ToList();

            Assert.AreEqual(2, result.Count);
            Assert.AreEqual(2, result[0].Count);

            Assert.AreEqual("O", result[0][0].From);
            Assert.AreEqual("A", result[0][0].To);
            Assert.AreEqual("Pattern1", result[0][0].Value);
            Assert.AreEqual("A", result[0][1].From);
            Assert.AreEqual("O", result[0][1].To);
            Assert.AreEqual("Pattern1", result[0][1].Value);

            Assert.AreEqual("O", result[1][0].From);
            Assert.AreEqual("A", result[1][0].To);
            Assert.AreEqual("Pattern2", result[1][0].Value);
            Assert.AreEqual("A", result[1][1].From);
            Assert.AreEqual("O", result[1][1].To);
            Assert.AreEqual("Pattern1", result[1][1].Value);
        }


        [Test]
        public void FindAllPaths()
        {
            DirectedGraph<int, string> graph = new DirectedGraph<int, string>();
            graph.AddVertex(0);
            graph.AddVertex(1);
            graph.AddVertex(2);
            graph.AddVertex(3);
            graph.AddEdge(0, 1, "01");
            graph.AddEdge(1, 2, "12");
            graph.AddEdge(0, 2, "02");
            graph.AddEdge(0, 3, "03");

            List<string> allPaths = graph.FindAllPaths(0, 2).Select(x => string.Join("-", x.Select(y => y))).ToList();
            Assert.AreEqual(2, allPaths.Count);
            Assert.AreEqual("0-1-2", allPaths[0]);
            Assert.AreEqual("0-2", allPaths[1]);
        }

        [Test]
        public void FindAllPaths_SelfReference()
        {
            DirectedGraph<string, string> graph = new DirectedGraph<string, string>();
            graph.AddVertex("A");
            graph.AddVertex("O");
            graph.AddVertex("I");
            graph.AddEdge("I", "O", "IO");
            graph.AddEdge("O", "O", "OO"); // to itself
            graph.AddEdge("O", "A", "OA");

            // Get all paths from I to A.
            var result = graph.FindAllPaths("I", "A").Select(x => string.Join("", x)).ToList();

            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("IOA", result[0]);
            Assert.AreEqual("IOOA", result[1]);
        }

        [Test]
        public void FindAllPaths_SelfReference_FirstElement()
        {
            DirectedGraph<string, string> graph = new DirectedGraph<string, string>();
            graph.AddVertex("A");
            graph.AddVertex("O");
            graph.AddEdge("A", "O", "AO");
            graph.AddEdge("A", "A", "AA"); // to itself

            // Get all paths from A to O.
            var result = graph.FindAllPaths("A", "O").Select(x => string.Join("", x)).ToList();

            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("AO", result[0]);
            Assert.AreEqual("AAO", result[1]);
        }

        [Test]
        public void FindAllPaths_SelfReference_LastElement()
        {
            DirectedGraph<string, string> graph = new DirectedGraph<string, string>();
            graph.AddVertex("A");
            graph.AddVertex("O");
            graph.AddEdge("A", "O", "AO");
            graph.AddEdge("O", "O", "OO"); // to itself

            // Get all paths from A to O.
            var result = graph.FindAllPaths("A", "O").Select(x => string.Join("", x)).ToList();

            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("AO", result[0]);
            Assert.AreEqual("AOO", result[1]);
        }
    }
}
