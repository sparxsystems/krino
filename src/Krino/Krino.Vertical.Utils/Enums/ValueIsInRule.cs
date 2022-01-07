using Krino.Vertical.Utils.Rules;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Evaluates true if Value is within the evaluated attributes.
    /// </summary>
    [DebuggerDisplay("{DebugView}")]
    public class ValueIsInRule : RuleBase<BigInteger>, IValueRule<BigInteger>
    {
        public static ValueIsInRule Is(BigInteger value) => new ValueIsInRule(value);

        public ValueIsInRule(BigInteger value)
        {
            Value = value;
        }

        public BigInteger Value { get; private set; }

        public override bool Evaluate(BigInteger attributes) => EnumBase.IsIn(Value, attributes);

        public override bool Equals(IRule<BigInteger> other) => other is ValueIsInRule maskRule && Value == maskRule.Value;

        public override int GetHashCode() => Value.GetHashCode();


        private string DebugView => Value.ToString();
    }
}
