using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class RuleTest
    {
        [Test]
        public void Or()
        {
            // if x == 10 || x == 20 || x == 30
            IRule<int> rule = Rule.Is(10).Or(20).Or(30);
            Assert.IsTrue(rule.Evaluate(10));
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsTrue(rule.Evaluate(30));
            Assert.IsFalse(rule.Evaluate(100));
        }

        [Test]
        public void And()
        {
            // if x != 10 && x == 20
            IRule<int> rule = Rule.Not(10).And(20);
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsFalse(rule.Evaluate(10));
            Assert.IsFalse(rule.Evaluate(100));
        }

        [Test]
        public void Not()
        {
            IRule<int> rule = Rule.Not(10);
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsFalse(rule.Evaluate(10));
        }
    }
}
