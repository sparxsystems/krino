using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    public class VerbFormAttributes : EnumGroupBase
    {
        public VerbFormAttributes(EnumGroupBase parent) : base(parent)
        {
            Base = new EnumValue(this);
            ThirdPersonSingular = new EnumValue(this);
            PresentParticiple = new EnumValue(this);
            Past = new EnumValue(this);
            PastParticiple = new EnumValue(this);
            Gerund = new EnumValue(this);
        }

        /// <summary>
        /// Infinitive form of the verb.
        /// (First form of the verb.)
        /// </summary>
        public EnumValue Base { get; }

        /// <summary>
        /// Third person singular form of a verb..
        /// </summary>
        public EnumBase ThirdPersonSingular { get; }

        /// <summary>
        /// The present participle verb form is created by adding -ing to the root word.
        /// It can also act as an adjective.
        /// </summary>
        /// <remarks>
        /// The present participle verb form is created by adding -ing to the root word.
        /// It’s used in the past, present, and future progressive verb tenses. 
        /// </remarks>
        public EnumValue PresentParticiple { get; }

        /// <summary>
        /// Past form of a verb.
        /// (Second form of the verb.)
        /// </summary>
        public EnumBase Past { get; }

        /// <summary>
        /// A verb form which is used in forming perfect and passive tenses and sometimes as an adjective.
        /// (Third form of the verb.)
        /// </summary>
        public EnumValue PastParticiple { get; }

        /// <summary>
        /// A verb form which acts as a noun. In English ending with -ing.
        /// </summary>
        /// <remarks>
        /// Gerunds are same as PresentParticiple - it differs only in their using.
        /// Gerunds are used as nouns.
        /// </remarks>
        public EnumValue Gerund { get; }
    }
}
