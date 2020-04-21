using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes
{
    /// <summary>
    /// Verb attributes.
    /// </summary>
    public class Verb : EnumGroupBase
    {
        public Verb(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 9)
        {
            Modal = new EnumValue(this, 1);
            Unergative = new EnumValue(this, 2);
            Unaccusative = new EnumValue(this, 3);
            Avalent = new EnumValue(this, 4);
            Monovalent = new EnumValue(this, 5);
            Bivalent = new EnumValue(this, 6);
            Trivalent = new EnumValue(this, 7);
            Quadrivalent = new EnumValue(this, 8);
            Pentavalent = new EnumValue(this, 9);
        }

        /// <summary>
        /// The verb is modal. e.g. may, can, will.
        /// </summary>
        public EnumValue Modal { get; }

        /// <summary>
        /// If stative does the verbant.
        /// </summary>
        public EnumValue Unergative { get; }

        /// <summary>
        /// Verbant happens to the actant (stative).
        /// </summary>
        public EnumValue Unaccusative { get; }

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
        public bool IsValencySpecified(ulong value)
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
        public int GetNumberOfValencies(ulong value)
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

            return 0;
        }
    }
}
