using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Structural
{
    public class VerbValencyAttributes : EnumGroupBase
    {
        public VerbValencyAttributes(EnumGroupBase parent) : base(parent)
        {
            Avalent = new EnumValue(this);
            Monovalent = new EnumValue(this);
            Bivalent = new EnumValue(this);
            Trivalent = new EnumValue(this);
            Quadrivalent = new EnumValue(this);
            Pentavalent = new EnumValue(this);
        }

        /// <summary>
        /// Valency 0.
        /// </summary>
        public EnumValue Avalent { get; }

        /// <summary>
        /// Valency 1.
        /// </summary>
        public EnumValue Monovalent { get; }

        /// <summary>
        /// Valency 2.
        /// </summary>
        public EnumValue Bivalent { get; }

        /// <summary>
        /// Valency 3.
        /// </summary>
        public EnumValue Trivalent { get; }

        /// <summary>
        /// Valency 4.
        /// </summary>
        public EnumValue Quadrivalent { get; }

        /// <summary>
        /// Valency 5.
        /// </summary>
        public EnumValue Pentavalent { get; }


        /// <summary>
        /// True if the value encodes at least one valancy attribute.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsValencySpecified(BigInteger value)
        {
            bool result = (Avalent & value) == Avalent ||
                (Monovalent & value) == Monovalent ||
                (Bivalent & value) == Bivalent ||
                (Trivalent & value) == Trivalent ||
                (Quadrivalent & value) == Quadrivalent ||
                (Pentavalent & value) == Pentavalent;

            return result;
        }

        /// <summary>
        /// Etract the valency attribute from the value and converts it to the number.
        /// </summary>
        /// <remarks>
        /// If there are encoded several valencies then it returns the lowest one.
        /// </remarks>
        /// <param name="value"></param>
        /// <returns></returns>
        public int GetNumberOfValencies(BigInteger value)
        {
            if (Avalent.IsIn(value))
            {
                return 0;
            }

            if (Monovalent.IsIn(value))
            {
                return 1;
            }

            if (Bivalent.IsIn(value))
            {
                return 2;
            }

            if (Trivalent.IsIn(value))
            {
                return 3;
            }

            if (Quadrivalent.IsIn(value))
            {
                return 4;
            }

            if (Pentavalent.IsIn(value))
            {
                return 5;
            }

            return -1;
        }
    }
}
