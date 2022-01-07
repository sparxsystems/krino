using Krino.Vertical.Utils.Enums;
using System;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AttributeTrigger : IEquatable<AttributeTrigger>
    {
        private ValueIsInRule myValueIsIn;

        public AttributeTrigger(BigInteger attributes)
        {
            myValueIsIn = new ValueIsInRule(attributes);
        }

        public BigInteger Attributes => myValueIsIn.Value;

        public bool Equals(AttributeTrigger other) => myValueIsIn.Evaluate(other.Attributes);

        public override bool Equals(object obj) => obj is AttributeTrigger trigger && Equals(trigger);

        // Note: it is not possible to achieve that if Equals return true both objects will have the same hash code.
        //       Therefore we need to relay on the Equals only.
        public override int GetHashCode() => 1;

        public static implicit operator AttributeTrigger(BigInteger attributes) => new AttributeTrigger(attributes);
        public static implicit operator AttributeTrigger(EnumBase attributes) => new AttributeTrigger(attributes);
    }
}
