using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Numerics;
using System.Text;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class Morpheme : LinguisticStructureBase, IMorpheme
    {
        private List<IMorpheme> mySuppletions = new List<IMorpheme>();

        public Morpheme(string value, BigInteger attributes)
            : base(attributes)
        {
            Value = value;
        }

        public string Value { get; private set; }

        public string GrammarStr => string.Join("", AttributesStr, "('", Value, "')");

        public void BuildFormattedGrammarStr(int indent, StringBuilder builder) => builder
            .Append(new string(' ', indent))
            .Append(Value).Append(" : ").AppendLine(AttributesStr);

        /// <summary>
        /// TODO: !!
        /// </summary>
        public GrammarCharacter GrammarCharacter => GrammarCharacter.e;

        public ILinguisticStructure DeepCopy() => new Morpheme(Value, Attributes);


        public IMorphemeBinding Binding { get; init; }


        public IMorpheme BaseForm { get; set; }

        public IMorpheme AddSuppletion(IMorpheme suppletion)
        {
            suppletion.BaseForm = this;
            mySuppletions.Add(suppletion);
            return this;
        }


        public IReadOnlyList<IMorpheme> Suppletions => mySuppletions;


        public bool Equals(IMorpheme other) =>
            Value == other.Value && Attributes == other.Attributes &&
            (Binding == other.Binding || Binding != null && other.Binding != null && Binding.Equals(other.Binding));

        public override bool Equals(object obj) => obj is Morpheme other && Equals(other);

        public static bool operator ==(Morpheme a, IMorpheme b) => a.Equals(b);

        public static bool operator !=(Morpheme a, IMorpheme b) => !(a == b);

        public override int GetHashCode() => HashCode.Combine(Value, Attributes, Binding);


        private string DebuggerDisplay => string.Join(" : ", Value, AttributesStr);
    }
}
