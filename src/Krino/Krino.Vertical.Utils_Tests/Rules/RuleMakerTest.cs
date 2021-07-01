using Krino.Vertical.Utils.Rules;
using NUnit.Framework;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class RuleMakerTest
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
        public void IsNot()
        {
            IRule<int> rule = RuleMaker.IsNot(10);
            Assert.IsTrue(rule.Evaluate(20));
            Assert.IsFalse(rule.Evaluate(10));
        }

        [Test]
        public void IsNotNull()
        {
            IRule<string> rule = RuleMaker.IsNotNull<string>();
            Assert.IsTrue(rule.Evaluate("hello"));
            Assert.IsFalse(rule.Evaluate(null));
        }

        [Test]
        public void ImplicitRuleOperators()
        {
            var rule = !RuleMaker.IsNull<string>() & !RuleMaker.Is("") & RuleMaker.Is("hi") | RuleMaker.Is("hello");
            Assert.IsTrue(rule.Evaluate("hi"));
            Assert.IsTrue(rule.Evaluate("hello"));
            Assert.IsFalse(rule.Evaluate("bla"));
            Assert.IsFalse(rule.Evaluate(null));
            Assert.IsFalse(rule.Evaluate(""));

            rule = !(RuleMaker.IsNull<string>() | RuleMaker.Is("")) & RuleMaker.Is("hi") | RuleMaker.Is("hello");
            Assert.IsTrue(rule.Evaluate("hi"));
            Assert.IsTrue(rule.Evaluate("hello"));
            Assert.IsFalse(rule.Evaluate("bla"));
            Assert.IsFalse(rule.Evaluate(null));
            Assert.IsFalse(rule.Evaluate(""));
        }

        [Test]
        public void Contains()
        {
            var rule1 = RuleMaker.Contains(null, "a", "b", "c");
            Assert.IsTrue(rule1.Evaluate("a"));
            Assert.IsTrue(rule1.Evaluate("b"));
            Assert.IsTrue(rule1.Evaluate("c"));
            Assert.IsFalse(rule1.Evaluate("d"));

            var rule2 = !RuleMaker.Contains(null, "a", "b", "c");
            Assert.IsFalse(rule2.Evaluate("a"));
            Assert.IsFalse(rule2.Evaluate("b"));
            Assert.IsFalse(rule2.Evaluate("c"));
            Assert.IsTrue(rule2.Evaluate("d"));
        }

        [Test]
        public void Expression()
        {
            var rule = RuleMaker.Expression<string>(x => x.StartsWith("h"));
            Assert.IsTrue(rule.Evaluate("hello"));
            Assert.IsFalse(rule.Evaluate("bla"));
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
