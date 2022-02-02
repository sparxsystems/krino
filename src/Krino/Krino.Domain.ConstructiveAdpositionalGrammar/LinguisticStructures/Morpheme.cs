﻿using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    internal class Morpheme : LinguisticStructureBase, IMorpheme
    {
        public Morpheme(string value, BigInteger attributes)
            : base(attributes)
        {
            Value = value;
        }

        public string Value { get; private set; }


        public bool Equals(IMorpheme other) => Value == other.Value && Attributes == other.Attributes;

        public override bool Equals(object obj) => obj is Morpheme other && Equals(other);

        public static bool operator ==(Morpheme a, IMorpheme b) => a.Equals(b);

        public static bool operator !=(Morpheme a, IMorpheme b) => !(a == b);

        public override int GetHashCode() => HashCode.Combine(Value, Attributes);


        private string DebuggerDisplay => string.Join(" : ", Value, AttributesStr);
    }
}
