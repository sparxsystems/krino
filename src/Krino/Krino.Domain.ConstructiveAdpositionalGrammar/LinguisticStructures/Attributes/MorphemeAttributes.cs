using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    public class MorphemeAttributes : EnumGroupBase
    {
        public MorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            O = new StativeAttributes(this);
            I = new VerbantAttributes(this);
            A = new AdjunctiveAttributes(this);
            E = new CircumstantialAttributes(this);
            U = new AdPositionAttributes(this);
        }

        /// <summary>
        /// Stative
        /// </summary>
        public StativeAttributes O { get; }

        /// <summary>
        /// Verbant
        /// </summary>
        public VerbantAttributes I { get; }

        /// <summary>
        /// Adjunctive
        /// </summary>
        public AdjunctiveAttributes A { get; }

        /// <summary>
        /// Circumstantial
        /// </summary>
        public CircumstantialAttributes E { get; }

        /// <summary>
        /// AdPosition
        /// </summary>
        public AdPositionAttributes U { get; }


        public bool IsFreeMorpheme(BigInteger attributes)
        {
            if (O.Free.IsIn(attributes) ||
                I.Free.IsIn(attributes) ||
                A.Free.IsIn(attributes) ||
                E.Free.IsIn(attributes) ||
                U.Free.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public bool IsPrefix(BigInteger attributes)
        {
            if (O.Bound.Prefix.IsIn(attributes) ||
                I.Bound.Prefix.IsIn(attributes) ||
                A.Bound.Prefix.IsIn(attributes) ||
                E.Bound.AdverbPrefix.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public bool IsSuffix(BigInteger attributes)
        {
            if (O.Bound.Suffix.IsIn(attributes) ||
                I.Bound.Suffix.IsIn(attributes) ||
                A.Bound.Suffix.IsIn(attributes) ||
                E.Bound.AdverbSuffix.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public GrammarCharacter GetGrammarCharacter(BigInteger attributes)
        {
            GrammarCharacter result;

            if (O.IsIn(attributes))
            {
                result = GrammarCharacter.O;
            }
            else if (I.IsIn(attributes))
            {
                result = GrammarCharacter.I;
            }
            else if (A.IsIn(attributes))
            {
                result = GrammarCharacter.A;
            }
            else if (E.IsIn(attributes))
            {
                result = GrammarCharacter.E;
            }
            else if (U.IsIn(attributes))
            {
                result = GrammarCharacter.U;
            }
            else
            {
                result = GrammarCharacter.e;
            }

            return result;
        }
    }
}
