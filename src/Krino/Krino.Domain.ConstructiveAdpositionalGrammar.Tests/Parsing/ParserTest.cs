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
            List<IMorpheme> lexemes = new List<IMorpheme>()
            {
                new Morpheme("I") { Attributes = Attributes.O.Pronoun.Subjective },
                new Morpheme("read") { Attributes = Attributes.I.Verb.Bivalent },
            };

            List<IPattern> patterns = new List<IPattern>()
            {
                new Pattern()
                {
                    MorphemeRule = MorphemeRule.O,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    MorphemeRule = MorphemeRule.I,
                    RightRule = PatternRule.Nothing,
                    LeftRule = PatternRule.Nothing,
                },

                new Pattern()
                {
                    PatternAttributes = PatternAttributes.ValencyPosition.First,
                    MorphemeRule = MorphemeRule.Epsilon,
                    RightRule = new PatternRule(MorphemeRule.I),
                    LeftRule = new PatternRule(MorphemeRule.O)
                },
            };


            ConstructiveDictionary dictionary = new ConstructiveDictionary(lexemes, patterns);

            Parser parser = new Parser(dictionary);
            IAdTree adTree = parser.Deserialize("I read", 0);

            Assert.AreEqual("I", adTree.Left.Morpheme.Morph);
            Assert.AreEqual("read", adTree.Right.Morpheme.Morph);
        }
    }
}
