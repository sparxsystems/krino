﻿using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Evaluates true if the evaluated attributes is within Value.
    /// </summary>
    [DebuggerDisplay("{DebugView}")]
    public class IsInValueRule : RuleBase<BigInteger>, IValueRule<BigInteger>
    {
        public static IsInValueRule Is(BigInteger value) => new IsInValueRule(value);

        public IsInValueRule(BigInteger value)
        {
            Value = value;
        }

        public BigInteger Value { get; private set; }

        public override bool Evaluate(BigInteger attributes) => EnumBase.IsIn(attributes, Value);

        public override bool Equals(IRule<BigInteger> other) => other is ValueIsInRule maskRule && Value == maskRule.Value;

        public override int GetHashCode() => Value.GetHashCode();


        private string DebugView => Value.ToString();
    }
}
