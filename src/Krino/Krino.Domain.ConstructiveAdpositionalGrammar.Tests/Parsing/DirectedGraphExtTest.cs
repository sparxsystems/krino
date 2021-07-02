using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.LinguisticConstructions;
using Krino.Domain.EnglishGrammar.LinguisticConstructions.Rules;
using Krino.Domain.EnglishGrammar.Morphemes;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Text;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
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
            var k1 = EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes.").GetHashCode();
            var k2 = EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme, "Rule accepting stative lexemes.").GetHashCode();
            var k3 = EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme).GetHashCode();
            var k4 = EnglishPattern.Morpheme(EnglishAttributes.O.Lexeme).GetHashCode();

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
        }
    }
}
