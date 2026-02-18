using System;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using SharpGen.Model;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace SharpGen.Generator.Marshallers;

internal sealed partial class ValueTypeArrayMarshaller
{
    private enum ArrayCopyDirection : byte
    {
        NativeToManaged,
        ManagedToNative
    }

    private static (SyntaxToken destination, SyntaxToken source) CopyDirectionToIdentifiers(
        ArrayCopyDirection direction, SyntaxToken managedName, SyntaxToken nativeName
    ) => direction switch
    {
        ArrayCopyDirection.NativeToManaged => (managedName, nativeName),
        ArrayCopyDirection.ManagedToNative => (nativeName, managedName),
        _ => throw new ArgumentOutOfRangeException(nameof(direction))
    };

    private StatementSyntax GenerateCopyMemory(CsMarshalBase marshallable, ArrayCopyDirection direction, bool takeAddress = true)
    {
        var (managed, native) = CopyDirectionToIdentifiers(
            direction, Identifier(ToIdentifier), Identifier(FromIdentifier)
        );

        var argumentExpression = marshallable.ArraySpecification?.Type == ArraySpecificationType.Constant ?
                    LiteralExpression(
                        SyntaxKind.NumericLiteralExpression,
                        Literal((uint) marshallable.ArrayDimensionValue)
                    ) :
                    GeneratorHelpers.CastExpression(TypeUInt32, GeneratorHelpers.LengthExpression(IdentifierName(marshallable.Name)));

        var copyInvocation = GenerateCopyMemoryInvocation(
            BinaryExpression(SyntaxKind.MultiplyExpression, argumentExpression, SizeOf(marshallable.MarshalType)),
            castTo: false, castFrom: false);

        var managedDecl = VariableDeclarator(managed, default, EqualsValueClause(PrefixUnaryExpression(SyntaxKind.AddressOfExpression,
            ElementAccessExpression(IdentifierName(marshallable.Name), BracketedArgumentList(SingletonSeparatedList(Argument(ZeroLiteral)))))));

        if (takeAddress)
        {
            var nativeDecl = VariableDeclarator(native, default, EqualsValueClause(PrefixUnaryExpression(SyntaxKind.AddressOfExpression, GetMarshalStorageLocation(marshallable))));
            return FixedStatement(VariableDeclaration(VoidPtrType, SeparatedList(new[] { managedDecl, nativeDecl })), copyInvocation);
        }

        var nativeLocal = LocalDeclarationStatement(VariableDeclaration(VoidPtrType, SingletonSeparatedList(
            VariableDeclarator(native, default, EqualsValueClause(GetMarshalStorageLocation(marshallable))))));

        return FixedStatement(VariableDeclaration(VoidPtrType, SingletonSeparatedList(managedDecl)), Block(nativeLocal, copyInvocation));
    }

    private StatementSyntax GenerateCopyBlock(CsMarshalCallableBase parameter, ArrayCopyDirection direction)
    {
        var arrayIdentifier = IdentifierName(parameter.Name);
        var marshalStorage = GetMarshalStorageLocationIdentifier(parameter);
        var fixedName = Identifier($"{marshalStorage}_");

        var (destination, source) = CopyDirectionToIdentifiers(direction, fixedName, marshalStorage);

        return FixedStatement(
            VariableDeclaration(
                VoidPtrType,
                SingletonSeparatedList(VariableDeclarator(fixedName, default, EqualsValueClause(arrayIdentifier)))
            ),
            GenerateCopyMemoryInvocation(
                IntPtrArgumentWithOptionalCast(IdentifierName(destination), true),
                IntPtrArgumentWithOptionalCast(IdentifierName(source), true),
                BinaryExpression(
                    SyntaxKind.MultiplyExpression,
                    GeneratorHelpers.CastExpression(TypeUInt32, GeneratorHelpers.LengthExpression(arrayIdentifier)),
                    SizeOf(parameter.PublicType)
                )
            )
        );
    }
}