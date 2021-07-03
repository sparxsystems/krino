using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class DirectedGraphExtTest
    {
        private EnglishAttributesModel myAttributesModel = new EnglishAttributesModel();

        [Test]
        public void GetPossibleAdTrees()
        {
            var patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes."),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme, "Rule accepting verbant lexemes."),
                EnglishPattern.Morpheme(EnglishAttributes.A.Lexeme, "Rule accepting adjunctive lexemes."),
                EnglishPattern.O1_I,
                EnglishPattern.EpsilonAdPosition("A-O", "Rule to add an adjunctive before a stative.", EnglishAttributes.A.Lexeme, EnglishAttributes.O.Lexeme),
            };

            var graph = patterns.CreatePatternGraph();

            var result = graph.GetPossibleAdTrees(EnglishPattern.O1_I, myAttributesModel, 2).ToList();
            Assert.AreEqual(1, result.Count);
            Assert.AreEqual("IO", result[0].GetPatternSignature());

            result = graph.GetPossibleAdTrees(EnglishPattern.O1_I, myAttributesModel, 3).ToList();
            Assert.AreEqual(2, result.Count);
            Assert.AreEqual("IO", result[0].GetPatternSignature());
            Assert.AreEqual("IOA", result[1].GetPatternSignature());


            patterns = new List<Pattern>()
            {
                EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes."),
                EnglishPattern.Morpheme(EnglishAttributes.I.Lexeme, "Rule accepting verbant lexemes."),
                EnglishPattern.O1_I,
                EnglishPattern.MorphematicAdPosition("O-U-O", "Connecting statives with a conjunction.", EnglishAttributes.U.Lexeme.Conjunction, EnglishAttributes.O.Lexeme, EnglishAttributes.O.Lexeme),
            };
            graph = patterns.CreatePatternGraph();

            result = graph.GetPossibleAdTrees(EnglishPattern.O1_I, myAttributesModel, 4).ToList();
            var k1 = result[0].GetPatternSignature();
            var k2 = result[1].GetPatternSignature();
            var k3 = result[2].GetPatternSignature();
            var k4 = result[3].GetPatternSignature();
        }
    }
}
