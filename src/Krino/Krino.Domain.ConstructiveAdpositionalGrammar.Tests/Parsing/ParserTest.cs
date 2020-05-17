using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using NUnit.Framework;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Parsing
{
    [TestFixture]
    public class ParserTest
    {
        [Test]
        public void Deseriaize()
        {
            List<Morpheme> lexemes = new List<Morpheme>()
            {
                new Morpheme("i") { Attributes = Attributes.O.Lexeme.Pronoun.Subjective },
                new Morpheme("read") { Attributes = Attributes.I.Lexeme.Verb.Bivalent },
            };

            List<Pattern> patterns = new List<Pattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I_Lexeme,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I_Lexeme),
                    LeftRule = new PatternRule(MorphemeRule.O_Lexeme)
                },
            };


            ConstructiveDictionary dictionary = new ConstructiveDictionary(lexemes, patterns);

            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I read", 0);

            Assert.AreEqual("i", adTree.Left.Morpheme.Morph);
            Assert.AreEqual("read", adTree.Right.Morpheme.Morph);
        }
    }
}
