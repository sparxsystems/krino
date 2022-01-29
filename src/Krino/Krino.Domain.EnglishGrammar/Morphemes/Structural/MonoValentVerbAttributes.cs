using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    public class MonoValentVerbAttributes : EnumGroupBase
    {
        public MonoValentVerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Unergative = new EnumValue(this);
            Unaccusative = new EnumValue(this);
        }

        /// <summary>
        /// If stative does the verbant.
        /// </summary>
        public EnumValue Unergative { get; }

        /// <summary>
        /// Verbant happens to the actant.
        /// </summary>
        public EnumValue Unaccusative { get; }
    }
}
