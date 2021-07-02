using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.ConstructiveDictionaries
{
    [TestFixture]
    public class PatternsExtTest
    {
        [Test]
        public void CreatePatternGraph()
        {
            var patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes."),
            };

            var graph = patterns.CreatePatternGraph();
            Assert.AreEqual(1, graph.Count);
            Assert.AreEqual(0, graph.Edges.Count());


            patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes."),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme, "Rule accepting verbant lexemes."),
                EnglishPattern.O1_I,
                EnglishPattern.O2_I,
            };

            graph = patterns.CreatePatternGraph();
            Assert.AreEqual(4, graph.Count);
            Assert.AreEqual(4, graph.Edges.Count());

            Assert.AreEqual(1, graph.GetEdgesGoingTo(EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme)));
            Assert.AreEqual(2, graph.GetEdgesGoingTo(EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme)));
            Assert.AreEqual(1, graph.GetEdgesGoingTo(EnglishPattern.O1_I));
            Assert.AreEqual(0, graph.GetEdgesGoingTo(EnglishPattern.O2_I));
        }
    }
}
