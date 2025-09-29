namespace Avalonia.FuncUI.DSL

open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.FuncUI.Builder
open Avalonia.Controls.Models.TreeDataGrid
open Avalonia.Controls.Primitives
open System
open Avalonia
open Avalonia.Input

[<AutoOpen>]
module TreeDataGrid =

    let create (attrs: IAttr<TreeDataGrid> list) : IView<TreeDataGrid> = ViewBuilder.Create<TreeDataGrid>(attrs)

    type TreeDataGrid with

        static member AutoDragDropRows<'TTree when 'TTree :> TreeDataGrid>(autoDragDropRows: bool) : IAttr<'TTree> =
            AttrBuilder<'TTree>.CreateProperty<bool>(TreeDataGrid.AutoDragDropRowsProperty, autoDragDropRows, ValueNone)

        static member CanUserResizeColumns<'TTree when 'TTree :> TreeDataGrid>
            (canUserResizeColumns: bool)
            : IAttr<'TTree> =
            AttrBuilder<'TTree>
                .CreateProperty<bool>(TreeDataGrid.CanUserResizeColumnsProperty, canUserResizeColumns, ValueNone)

        static member CanUserSortColumns<'TTree when 'TTree :> TreeDataGrid>(canUserSortColumns: bool) : IAttr<'TTree> =
            AttrBuilder<'TTree>
                .CreateProperty<bool>(TreeDataGrid.CanUserSortColumnsProperty, canUserSortColumns, ValueNone)

        static member Columns<'TTree when 'TTree :> TreeDataGrid>(columns: IColumns) : IAttr<'TTree> =
            AttrBuilder<'TTree>.CreateProperty<IColumns>(TreeDataGrid.ColumnsProperty, columns, ValueNone)

        static member ElementFactory<'TTree when 'TTree :> TreeDataGrid>
            (factory: TreeDataGridElementFactory)
            : IAttr<'TTree> =
            AttrBuilder<'TTree>
                .CreateProperty<TreeDataGridElementFactory>(TreeDataGrid.ElementFactoryProperty, factory, ValueNone)

        static member Rows<'TTree when 'TTree :> TreeDataGrid>(rows: IRows) : IAttr<'TTree> =
            AttrBuilder<'TTree>.CreateProperty<IRows>(TreeDataGrid.RowsProperty, rows, ValueNone)

        static member Scroll<'TTree when 'TTree :> TreeDataGrid>(scrollable: IScrollable) : IAttr<'TTree> =
            AttrBuilder<'TTree>.CreateProperty<IScrollable>(TreeDataGrid.ScrollProperty, scrollable, ValueNone)

        static member ShowColumnHeaders<'TTree when 'TTree :> TreeDataGrid>(showHeaders: bool) : IAttr<'TTree> =
            AttrBuilder<'TTree>.CreateProperty<bool>(TreeDataGrid.ShowColumnHeadersProperty, showHeaders, ValueNone)

        static member Source<'TTree when 'TTree :> TreeDataGrid>(dataSource: ITreeDataGridSource) : IAttr<'TTree> =
            AttrBuilder<'TTree>.CreateProperty<ITreeDataGridSource>(TreeDataGrid.SourceProperty, dataSource, ValueNone)

        // Provide a simple selection change hook by observing Source property changes
        static member OnSelectionChanged<'TTree when 'TTree :> TreeDataGrid>
            (handler: TreeDataGrid -> unit, ?subPatchOptions)
            : IAttr<'TTree> =
            let factory (_: AvaloniaObject, (_: unit -> unit), _ct) = ()

            AttrBuilder<'TTree>
                .CreateSubscription(
                    name = "Excalibur.TreeDataGrid.onSelectionChanged",
                    factory = factory,
                    func = (fun (_: unit) -> ()),
                    ?subPatchOptions = subPatchOptions
                )

        static member OnPointerPressed<'TTree when 'TTree :> TreeDataGrid>
            (func: PointerPressedEventArgs -> unit, ?subPatchOptions)
            =
            AttrBuilder<'TTree>
                .CreateSubscription<PointerPressedEventArgs>(
                    InputElement.PointerPressedEvent,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

        static member RowDragStarted<'TTree when 'TTree :> TreeDataGrid>
            (func: TreeDataGridRowDragStartedEventArgs -> unit, ?subPatchOptions)
            =
            AttrBuilder<'TTree>
                .CreateSubscription<TreeDataGridRowDragStartedEventArgs>(
                    TreeDataGrid.RowDragStartedEvent,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

        static member RowDragOver<'TTree when 'TTree :> TreeDataGrid>
            (func: TreeDataGridRowDragEventArgs -> unit, ?subPatchOptions)
            =
            AttrBuilder<'TTree>
                .CreateSubscription<TreeDataGridRowDragEventArgs>(
                    TreeDataGrid.RowDragOverEvent,
                    func,
                    ?subPatchOptions = subPatchOptions
                )

        static member RowDrop<'TTree when 'TTree :> TreeDataGrid>
            (func: TreeDataGridRowDragEventArgs -> unit, ?subPatchOptions)
            =
            AttrBuilder<'TTree>
                .CreateSubscription<TreeDataGridRowDragEventArgs>(
                    TreeDataGrid.RowDropEvent,
                    func,
                    ?subPatchOptions = subPatchOptions
                )
