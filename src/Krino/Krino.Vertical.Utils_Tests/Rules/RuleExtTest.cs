using Krino.Vertical.Utils.Rules;
using NUnit.Framework;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils_Tests.Rules
{
    [TestFixture]
    public class RuleExtTest
    {
        [Test]
        public void GetRules()
        {
            // !((1 && 2) || 3)
            // it means:  Not(Or(And(1, 2), 3)) => 6 rules: Not, Or, And, 1, 2, 3.
            IRule<int> rule = RuleMaker.Is(1).And(2).Or(3).Not();
            List<IRule<int>> rules = rule.GetRules().ToList();
            Assert.AreEqual(6, rules.Count);
            Assert.IsTrue(rules[0] is NotRule<int>);
            Assert.IsTrue(rules[1] is OrRule<int>);
            Assert.IsTrue(rules[2] is AndRule<int>);
            Assert.IsTrue(rules[3] is IsRule<int> is1 && is1.ReferenceValue == 1);
            Assert.IsTrue(rules[4] is IsRule<int> is2 && is2.ReferenceValue == 2);
            Assert.IsTrue(rules[5] is IsRule<int> is3 && is3.ReferenceValue == 3);
        }

        [Test]
        public void GetReferenceValues()
        {
            // !((1 && 2) || 3)
            // it means:  Not(Or(And(1, 2), 3)) => 6 rules: Not, Or, And, 1, 2, 3.
            IRule<int> rule = RuleMaker.Is(1).And(2).Or(3).Not();

            List<int> rules = rule.GetReferenceValues().ToList();
            Assert.AreEqual(3, rules.Count);
            Assert.AreEqual(1, rules[0]);
            Assert.AreEqual(2, rules[1]);
            Assert.AreEqual(3, rules[2]);
        }

        [Test]
        public void IsSubruleOf()
        {
            IRule<int> rule1 = RuleMaker.Is(1) | RuleMaker.Is(2);
            IRule<int> rule2 = RuleMaker.Is(1) | RuleMaker.Is(2) | RuleMaker.Is(3);

            Assert.IsTrue(rule1.IsSubruleOf(rule2));
            Assert.IsFalse(rule2.IsSubruleOf(rule1));

            // Two same rules.
            Assert.IsTrue(rule1.IsSubruleOf(rule1));

            Assert.IsTrue(rule1.IsSubruleOf(RuleMaker.Anything<int>()));
            Assert.IsFalse(rule1.IsSubruleOf(RuleMaker.Nothing<int>()));

            Assert.IsTrue(RuleMaker.Anything<int>().IsSubruleOf(RuleMaker.Anything<int>()));

            Assert.IsFalse(RuleMaker.Anything<int>().IsSubruleOf(RuleMaker.Nothing<int>()));

            Assert.IsFalse(RuleMaker.Nothing<int>().IsSubruleOf(RuleMaker.Anything<int>()));
        }

        [Test]
        public void IsSubruleOf_Negations()
        {
            IRule<int> rule1 = !RuleMaker.Is(1) & !RuleMaker.Is(2);
            IRule<int> rule2 = !RuleMaker.Is(1) & !RuleMaker.Is(2) & !RuleMaker.Is(3);

            Assert.IsTrue(rule2.IsSubruleOf(rule1));
            Assert.IsFalse(rule1.IsSubruleOf(rule2));
        }
    }
}
