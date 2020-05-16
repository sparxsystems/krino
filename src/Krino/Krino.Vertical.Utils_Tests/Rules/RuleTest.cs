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
            IRule<int> rule = RuleMaker.Is(10).Or(20).Or(30);
            Assert.IsTrue(rule.Evaluate(10));
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsTrue(rule.Evaluate(30));
            Assert.IsFalse(rule.Evaluate(100));
        }

        [Test]
        public void And()
        {
            // if x != 10 && x == 20
            IRule<int> rule = RuleMaker.Not(10).And(20);
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsFalse(rule.Evaluate(10));
            Assert.IsFalse(rule.Evaluate(100));
        }

        [Test]
        public void Not()
        {
            IRule<int> rule = RuleMaker.Not(10);
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsFalse(rule.Evaluate(10));
        }

        [Test]
        public void Equals()
        {
            IRule<int> rule1 = RuleMaker.Is(10).Or(20).Or(30);
            IRule<int> rule2 = RuleMaker.Is(10).Or(20).Or(30);
            Assert.IsTrue(rule1.Equals(rule2));
            Assert.IsTrue(rule2.Equals(rule1));

            // Different order means a different rule because the evaluation may stop in different point.
            // E.g. if the second OR is true then it does not continue. So both rules although giving the same result will
            //      be evaluated differently.
            rule2 = RuleMaker.Is(10).Or(30).Or(20);
            Assert.IsFalse(rule1.Equals(rule2));


            rule1 = RuleMaker.Is(10).And(20).And(30);
            rule2 = RuleMaker.Is(10).And(20).And(30);
            Assert.IsTrue(rule1.Equals(rule2));
            Assert.IsTrue(rule2.Equals(rule1));

            rule2 = RuleMaker.Is(10).And(30).And(20);
            Assert.IsFalse(rule1.Equals(rule2));


            rule1 = RuleMaker.Is(10).Not();
            rule2 = RuleMaker.Is(10).Not();
            Assert.IsTrue(rule1.Equals(rule2));

            rule1 = RuleMaker.Is(10).Not();
            rule2 = RuleMaker.Is(20).Not();
            Assert.IsFalse(rule1.Equals(rule2));



            rule1 = RuleMaker.Anything<int>();
            rule2 = RuleMaker.Anything<int>();
            Assert.IsTrue(rule1.Equals(rule2));


            rule1 = RuleMaker.Nothing<int>();
            rule2 = RuleMaker.Nothing<int>();
            Assert.IsTrue(rule1.Equals(rule2));
        }
    }
}
