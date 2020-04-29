using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Tests.Constructions
{
    [TestFixture]
    public class MorphemeRuleTest
    {
        [Test]
        public void Anything()
        {
            Assert.IsTrue(MorphemeRule.Anything.IsMatch("", 0));
            Assert.IsTrue(MorphemeRule.Anything.IsMatch(null, 0));
            Assert.IsTrue(MorphemeRule.Anything.IsMatch("bla", ulong.MaxValue));
        }

        [Test]
        public void Nothing()
        {
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch("", 0));
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch(null, 0));
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch("bla", ulong.MaxValue));
            Assert.IsFalse(MorphemeRule.Nothing.IsMatch("bla", 123));
        }

        [Test]
        public void IsMatch()
        {
            MorphemeRule morphemeRule = new MorphemeRule(Rule.Is("hello"), MaskRule.Is(2ul).And(MaskRule.Is(4ul).Not()));
            Assert.IsTrue(morphemeRule.IsMatch("hello", 3));
            Assert.IsFalse(morphemeRule.IsMatch("bla", 3));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 5));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 6));


            morphemeRule = new MorphemeRule(Rule.Anything<string>(), MaskRule.Is(2ul).And(MaskRule.Is(4ul).Not()));
            Assert.IsTrue(morphemeRule.IsMatch("hello", 3));
            Assert.IsTrue(morphemeRule.IsMatch("bla", 3));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 5));
            Assert.IsFalse(morphemeRule.IsMatch("hello", 6));
        }
    }
}
