using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class PatternRuleTest
    {
        [Test]
        public void GetMatchingGrammarCharacters()
        {
            PatternRule rule = new PatternRule(MorphemeRule.I);

            List<GrammarCharacter> grammarCharacters = rule.GetMatchingGrammarCharacters().ToList();
            Assert.AreEqual(1, grammarCharacters.Count);
            Assert.AreEqual(GrammarCharacter.I, grammarCharacters[0]);


            rule = new PatternRule(new MorphemeRule(Rule.Anything<string>(),
                Rule.Is<BigInteger>(Attributes.A).Or(Rule.Is<BigInteger>(Attributes.O))));
            grammarCharacters = rule.GetMatchingGrammarCharacters().ToList();
            Assert.AreEqual(2, grammarCharacters.Count);
            Assert.IsTrue(grammarCharacters.Contains(GrammarCharacter.A));
            Assert.IsTrue(grammarCharacters.Contains(GrammarCharacter.O));
        }

        [Test]
        public void IsMatch_Anything()
        {
            PatternRule rule = PatternRule.Anything;
            Assert.IsTrue(rule.IsMatch("", 0, 0));
            Assert.IsTrue(rule.IsMatch(null, 0, 0));
            Assert.IsTrue(rule.IsMatch("hello", 0, 0));
            Assert.IsTrue(rule.IsMatch("hello", 100, 50));
        }

        [Test]
        public void IsMatch()
        {
            // Pattern rule which matches any morpheme and requires the mask where the 2nd bit is set and the fourth bit is not set.
            PatternRule rule = new PatternRule(MorphemeRule.Anything, MaskRule.Is(2ul).And(Rule.Is((BigInteger)4).Not()));
            Assert.IsTrue(rule.IsMatch("", 0, 3));
            Assert.IsTrue(rule.IsMatch(null, 0, 3));
            Assert.IsTrue(rule.IsMatch("hello", 0, 3));
            Assert.IsTrue(rule.IsMatch("hello", 100, 3));
            Assert.IsFalse(rule.IsMatch("", 0, 5));
            Assert.IsFalse(rule.IsMatch(null, 0, 5));
            Assert.IsFalse(rule.IsMatch("hello", 100, 5));
        }
    }
}
