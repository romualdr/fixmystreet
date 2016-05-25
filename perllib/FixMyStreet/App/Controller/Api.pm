package FixMyStreet::App::Controller::Api;
use Moose;
use namespace::autoclean;

use File::Slurp;
use List::MoreUtils qw(any);
use POSIX qw(strcoll);
use RABX;
use JSON;
use Utils;

BEGIN { extends 'Catalyst::Controller'; }

=head2 index

Display contact us page

=cut

sub index : Path : Args(0) {
    my ( $self, $c ) = @_;

    my $body = JSON->new->utf8(1)->encode(
        {
            category_extra => 'yol11o',
        }
    );
    $c->res->content_type('application/json; charset=utf-8');
    $c->res->body($body);

    return 1;
}

sub problem_as_hashref : Private {
    my $c = shift;
    my $problem = shift;

    my @photos = photos_lite($problem);

    return {
        id        => $problem->id,
        title     => $problem->title,
        category  => $problem->category,
        detail    => $problem->detail,
        latitude  => $problem->latitude,
        longitude => $problem->longitude,
        postcode  => $problem->postcode,
        state     => $problem->state,
        state_t   => _( $problem->state ),
        used_map  => $problem->used_map,
        is_fixed  => $problem->fixed_states->{ $problem->state } ? 1 : 0,
        photo     => (@photos == 0 ? () : $photos[0][0]),
        meta      => $problem->confirmed ? $problem->meta_line( $c ) : '',
        confirmed => $problem->confirmed ? $problem->confirmed->strftime('%H:%M %Y-%m-%d'): '',
        created   => $problem->created ? $problem->created->strftime('%H:%M %Y-%m-%d') : '',
        creator   => $problem->name ? $problem->name : '',
    };
}


=head2 photos_lite

Lite photo problem extractor for API
Code taken from /perllib/FixMyStreet/DB/Result/Problem.pm:photos
Edited a little bit

=cut
sub photos_lite : Private {
    my $problem = shift;
    my $photoset = $problem->get_photoset;
    my $id = $problem->id;

    my $i = 0;
    my @photos = map {
        my $cachebust = substr($_, 0, 8);
        my ($hash, $format) = split /\./, $_;
        {
            url => "/photo/$id.$i.$format?$cachebust",
            url_full => "/photo/$id.$i.full.$format?$cachebust",
            url_tn => "/photo/$id.$i.tn.$format?$cachebust",
            url_fp => "/photo/$id.$i.fp.$format?$cachebust"
        }
    } $photoset->all_ids;
    return \@photos;
}


sub all_reports : Path('councils') : Args(0){
	my ( $self, $c ) = @_;

    my $p_page = $c->req->params->{p} || 1;
    my $u_rows = $c->req->params->{rows} || 50;

    # Fetch all bodies
    my @bodies = $c->model('DB::Body')->search({}, {
        '+select' => [ { count => 'area_id' } ],
        '+as' => [ 'area_count' ],
        join => 'body_areas',
        distinct => 1,
    })->all;
    @bodies = sort { strcoll($a->name, $b->name) } @bodies;
    $c->stash->{bodies} = \@bodies;
    $c->stash->{any_empty_bodies} = any { $_->get_column('area_count') == 0 } @bodies;

    my $json_bodies = [];

    foreach my $body (@bodies){
    	push @$json_bodies, {
    		name => $body->name,
    		areas  => $body->areas,
    	};
    }



    eval {
        my $data = File::Slurp::read_file(
            FixMyStreet->path_to( '../data/all-reports.json' )->stringify
        );
        my $j = JSON->new->utf8(1)->decode($data);

       	my $json = JSON->new->utf8(1)->encode(
	        {
	            bodies => $json_bodies,
	            data => $j,
	        }
	    );

        $c->res->body($json);
        $c->res->content_type('application/json; charset=utf-8');
        $c->stash->{fixed} = $j->{fixed};
        $c->stash->{open} = $j->{open};
    };

    if ($@) {
        my $error = JSON->new->utf8(1)->encode(
            {
                error => 'ERROR_COUNCILS',
                msg   => 'Error getting councils',
            }
        );

        $c->res->body($error);
        $c->res->content_type('application/json; charset=utf-8');

        # Down here so that error pages aren't cached.
        $c->response->header('Cache-Control' => 'max-age=3600');
        return 1;
    }
}

sub my_reports : Path('my_reports') : Args(0) {
    my ( $self, $c ) = @_;

    my $json;
	my $p_page = $c->req->params->{p} || 1;
    my $u_page = $c->req->params->{u} || 1;
    my $u_rows = $c->req->params->{rows} || 50;

    my $pins = [];
    my $problems = {};

    my $params = {
        'me.state' => [ FixMyStreet::DB::Result::Problem->visible_states() ],
    };

    if ( $c->forward( '/auth/sign_in' ) ) {

	    my $rs = $c->user->problems->search( $params, {
	        order_by => { -desc => 'created' },
	        rows => $u_rows,
	    })->page( $p_page );


	    while ( my $problem = $rs->next ) {
	        $c->stash->{has_content}++;

            my $updates = $c->model('DB::Comment')->search(
                { problem_id => $problem->id, state => 'confirmed' },
                { order_by => 'confirmed' }
            );

            my $updates_json = [];
            while(my $com = $updates->next){

                push @$updates_json,{
                    text => $com->text,
                    name => $com->name,
                    created => $com->created->strftime('%H:%M %Y-%m-%d'),
                }
            }

            my $problem_formatted = problem_as_hashref($c, $problem);
            my $comments = {
                comments => $updates_json,
            };

	        push @$pins, { %$problem_formatted, %$comments};
	        #my $state = $problem->is_fixed ? 'fixed' : $problem->is_closed ? 'closed' : 'confirmed';
	        #push @{ $problems->{$state} }, $problem;
	    }
	    $c->stash->{problems_pager} = $rs->pager;
	    $c->stash->{problems} = $problems;

	    $rs = $c->user->comments->search(
	        { state => 'confirmed' },
	        {
	            order_by => { -desc => 'confirmed' },
	            rows => 50
	        } )->page( $u_page );

	    my @updates = $rs->all;
	    $c->stash->{has_content} += scalar @updates;
	    $c->stash->{updates} = \@updates;
	    $c->stash->{updates_pager} = $rs->pager;

	   	$json = JSON->new->utf8(1)->allow_blessed->convert_blessed->encode(
	        {
	            reports => $pins,
	        }
	    );

    }else{

	    $json = JSON->new->utf8(1)->encode(
	        {
	            error => 'login failed',
	        }
	    );
    }

    $c->res->content_type('application/json; charset=utf-8');
    $c->res->body($json);

    return 1;
}

sub council_reports : Path('reports/') : Args(1){
    my ( $self, $c, $body, $ward ) = @_;

    my $json;
    $body = str_replace(' ','+', $body);

    eval{
		$c->forward( 'body_check', [ $body ] );
		$c->forward( 'ward_check', [ $ward ] ) if $ward;

		if($c->stash->{body}){

            if($c->forward( 'check_canonical_url', [ $body ] )){

                $c->forward( 'load_and_group_problems' );

                my $pins = $c->stash->{pins};
                my $problems = $c->stash->{problems};


                $json = JSON->new->utf8(1)->encode(
                    {
                        reports => $pins,
                        #problems => $problems,
                    }
                );

            }else{

                $json = JSON->new->utf8(1)->encode(
                    {
                        error => 'council not found',
                    }
                );
            }

		}else{

			$json = JSON->new->utf8(1)->encode(
		        {
		            error => 'council not found',
		        }
		    );
		}
    };

    $c->res->content_type('application/json; charset=utf-8');
    $c->res->body($json);

}

=head2 body_check

This action checks the body name (or code) given in a URI exists, is valid and
so on. If it is, it stores the body in the stash, otherwise it redirects to the
all reports page.

=cut

sub body_check : Private {
    my ( $self, $c, $q_body ) = @_;

    $q_body =~ s/\+/ /g;
    $q_body =~ s/\.html//;

    # Check cobrand specific incantations - e.g. ONS codes for UK,
    # Oslo/ kommunes sharing a name in Norway
    return if $c->cobrand->reports_body_check( $c, $q_body );

    # If we're passed an ID number (don't think this is used anywhere, it
    # certainly shouldn't be), just look that up on MaPit and redirect
    if ($q_body =~ /^\d+$/) {
        my $area = mySociety::MaPit::call('area', $q_body);
        $c->detach( 'redirect_index') if $area->{error};
        $c->stash->{body} = $area;
        return;
    }

    # We must now have a string to check
    my @bodies = $c->model('DB::Body')->search( { name => { -like => "$q_body%" } } )->all;

    if (@bodies == 1) {
        $c->stash->{body} = $bodies[0];
        return;
    } else {
        foreach (@bodies) {
            if (lc($_->name) eq lc($q_body) || $_->name =~ /^\Q$q_body\E (Borough|City|District|County) Council$/i) {
                $c->stash->{body} = $_;
                return;
            }
        }
    }

    return;
}

=head2 ward_check

This action checks the ward name from a URI exists and is part of the right
parent, already found with body_check. It either stores the ward Area if
okay, or redirects to the body page if bad.

=cut

sub ward_check : Private {
    my ( $self, $c, $ward ) = @_;

    $ward =~ s/\+/ /g;
    $ward =~ s/\.html//;
    $ward =~ s{_}{/}g;

    # Could be from RSS area, or body...
    my $parent_id;
    if ( $c->stash->{body} ) {
        $parent_id = $c->stash->{body}->body_areas->first;
        if($parent_id){
            $parent_id = $parent_id->area_id;
        }else{
            return;
        }

    } else {
        $parent_id = $c->stash->{area}->{id};
    }

    my $qw = mySociety::MaPit::call('areas', $ward,
        type => $c->cobrand->area_types_children,
    );
    foreach my $area (sort { $a->{name} cmp $b->{name} } values %$qw) {
        if ($area->{parent_area} == $parent_id) {
            $c->stash->{ward} = $area;
            return;
        }
    }
    # Given a false ward name
    $c->stash->{body} = $c->stash->{area}
        unless $c->stash->{body};
    #$c->detach( 'redirect_body' );
    return;
}

=head2 check_canonical_url

Given an already found (case-insensitively) body, check what URL
we are at and redirect accordingly if different.

=cut

sub check_canonical_url : Private {
    my ( $self, $c, $q_body ) = @_;

    my $body_short = $c->cobrand->short_name( $c->stash->{body} );
    my $url_short = URI::Escape::uri_escape_utf8($q_body);
    $url_short =~ s/%2B/+/g;

    return $body_short eq $url_short;
    #$c->detach( 'redirect_body' ) unless $body_short eq $url_short;
}

sub load_and_group_problems : Private {
    my ( $self, $c ) = @_;

    my $u_rows = $c->req->params->{rows} || $c->cobrand->reports_per_page;
    my $page = $c->req->params->{p} || 1;
    my $type = $c->req->params->{t} || 'all';
    my $category = $c->req->params->{c} || '';

    my $where = {
        non_public => 0,
        state      => [ FixMyStreet::DB::Result::Problem->visible_states() ]
    };

    my $not_open = [ FixMyStreet::DB::Result::Problem::fixed_states(), FixMyStreet::DB::Result::Problem::closed_states() ];
    if ( $type eq 'new' ) {
        $where->{confirmed} = { '>', \"ms_current_timestamp() - INTERVAL '4 week'" };
        $where->{state} = { 'IN', [ FixMyStreet::DB::Result::Problem::open_states() ] };
    } elsif ( $type eq 'older' ) {
        $where->{confirmed} = { '<', \"ms_current_timestamp() - INTERVAL '4 week'" };
        $where->{lastupdate} = { '>', \"ms_current_timestamp() - INTERVAL '8 week'" };
        $where->{state} = { 'IN', [ FixMyStreet::DB::Result::Problem::open_states() ] };
    } elsif ( $type eq 'unknown' ) {
        $where->{lastupdate} = { '<', \"ms_current_timestamp() - INTERVAL '8 week'" };
        $where->{state} = { 'IN',  [ FixMyStreet::DB::Result::Problem::open_states() ] };
    } elsif ( $type eq 'fixed' ) {
        $where->{lastupdate} = { '>', \"ms_current_timestamp() - INTERVAL '8 week'" };
        $where->{state} = $not_open;
    } elsif ( $type eq 'older_fixed' ) {
        $where->{lastupdate} = { '<', \"ms_current_timestamp() - INTERVAL '8 week'" };
        $where->{state} = $not_open;
    }

    if ($category) {
        $where->{category} = $category;
    }

    if ($c->stash->{ward}) {
        $where->{areas} = { 'like', '%,' . $c->stash->{ward}->{id} . ',%' };
        $where->{bodies_str} = [
            undef,
            $c->stash->{body}->id,
            { 'like', $c->stash->{body}->id . ',%' },
            { 'like', '%,' . $c->stash->{body}->id },
        ];
    } elsif ($c->stash->{body}) {
        # XXX FixMyStreet used to have the following line so that reports not
        # currently sent anywhere could still be listed in the appropriate
        # (body/area), as they were the same.  Now they're not, not sure if
        # there's a way to do this easily.
        #$where->{areas} = { 'like', '%,' . $c->stash->{body}->id . ',%' };
        $where->{bodies_str} = [
        #    undef,
            $c->stash->{body}->id,
            { 'like', $c->stash->{body}->id . ',%' },
            { 'like', '%,' . $c->stash->{body}->id },
        ];
    }

    my $problems = $c->cobrand->problems->search(
        $where,
        {
            order_by => { -desc => 'lastupdate' },
            rows => $u_rows,
        }
    )->page( $page );
    $c->stash->{pager} = $problems->pager;

    my ( %problems, @pins );
    while ( my $problem = $problems->next ) {
        $c->log->debug( $problem->cobrand . ', cobrand is ' . $c->cobrand->moniker );
        if ( !$c->stash->{body} ) {
            add_row( $c, $problem, 0, \%problems, \@pins );
            next;
        }
        if ( !$problem->bodies_str ) {
            # Problem was not sent to any body, add to all possible areas XXX
            my $a = $problem->areas; # Store, as otherwise is looked up every iteration.
            while ($a =~ /,(\d+)(?=,)/g) {
                add_row( $c, $problem, $1, \%problems, \@pins );
            }
        } else {
            # Add to bodies it was sent to
            # XXX Assumes body ID matches "council ID"
            my $bodies = $problem->bodies_str_ids;
            foreach ( @$bodies ) {
                next if $_ != $c->stash->{body}->id;
                add_row( $c, $problem, $_, \%problems, \@pins );
            }
        }
    }

    $c->stash(
        problems      => \%problems,
        pins          => \@pins,
    );

    return 1;
}


sub add_row {
    my ( $c, $problem, $body, $problems, $pins ) = @_;
    push @{$problems->{$body}}, $problem;
    push @$pins, problem_as_hashref($c, $problem);
}

=head2 report_display

Display a report.

=cut

sub display : Path('report/') : Args(1) {
    my ( $self, $c, $id ) = @_;

    if (
        $id =~ m{ ^ 3D (\d+) $ }x         # Some council with bad email software
        || $id =~ m{ ^(\d+) \D .* $ }x    # trailing garbage
      )
    {
	    $c->res->content_type('application/json; charset=utf-8');
	    my $content = JSON->new->utf8(1)->allow_blessed->convert_blessed->encode(
	        {
	            error => 'report not found.'
	        }
	    );
		$c->res->body( $content );
		return 1;
    }

    $c->forward( '_display', [ $id ] );
}

sub _display : Private {
    my ( $self, $c, $id ) = @_;

    if($c->forward( 'load_problem_or_display_error', [ $id ] )){
	    $c->forward( 'load_updates' );
    	$c->forward( 'format_problem_for_display' );
    }else{
	    $c->res->content_type('application/json; charset=utf-8');
	    my $content = JSON->new->utf8(1)->allow_blessed->convert_blessed->encode(
	        {
	            error => 'report not found.'
	        }
	    );
		$c->res->body( $content );
		return 1;
    }


}

sub load_problem_or_display_error : Private {
    my ( $self, $c, $id ) = @_;

    # try to load a report if the id is a number
    my $problem
      = ( !$id || $id =~ m{\D} ) # is id non-numeric?
      ? undef                    # ...don't even search
      : $c->cobrand->problems->find( { id => $id } );

    # check that the problem is suitable to show.
    if ( !$problem || ($problem->state eq 'unconfirmed' && !$c->cobrand->show_unconfirmed_reports) || $problem->state eq 'partial' ) {
        return;
    }
    elsif ( $problem->state eq 'hidden' ) {
        return;
    } elsif ( $problem->non_public ) {
    	return;
    }

    $c->stash->{problem} = $problem;
    return 1;
}

sub load_updates : Private {
    my ( $self, $c ) = @_;

    my $updates = $c->model('DB::Comment')->search(
        { problem_id => $c->stash->{problem}->id, state => 'confirmed' },
        { order_by => 'confirmed' }
    );

    my $questionnaires = $c->model('DB::Questionnaire')->search(
        {
            problem_id => $c->stash->{problem}->id,
            whenanswered => { '!=', undef },
            old_state => 'confirmed', new_state => 'confirmed',
        },
        { order_by => 'whenanswered' }
    );


    my @combined;
    my $updateslist;
    while (my $update = $updates->next) {
        push @combined, [ $update->confirmed, $update ];
        push @$updateslist, {
        	name      => $update->name,
        	text 	  => $update->text,
        	photo 	  => photos_lite($update),
        	confirmed => $update->confirmed ? $update->confirmed->strftime('%H:%M %Y-%m-%d') :'',
            created   => $update->created ? $update->created->strftime('%H:%M %Y-%m-%d') : '',
        };
    }
    while (my $update = $questionnaires->next) {
        push @combined, [ $update->whenanswered, $update ];
    }
    @combined = map { $_->[1] } sort { $a->[0] <=> $b->[0] } @combined;
    $c->stash->{updates} = $updateslist;

    return 1;
}

sub format_problem_for_display : Private {
    my ( $self, $c ) = @_;

    my $problem = $c->stash->{problem};
    my $updates = $c->stash->{updates};

    ( $c->stash->{short_latitude}, $c->stash->{short_longitude} ) =
      map { Utils::truncate_coordinate($_) }
      ( $problem->latitude, $problem->longitude );

    unless ( $c->req->param('submit_update') ) {
        $c->stash->{add_alert} = 1;
    }

    $c->stash->{extra_name_info} = $problem->bodies_str && $problem->bodies_str eq '2482' ? 1 : 0;
    if ( $c->sessionid && $c->flash->{created_report} ) {
        $c->stash->{created_report} = $c->flash->{created_report};
    }

    $c->forward('generate_map_tags');


    $c->res->content_type('application/json; charset=utf-8');
    my $content = JSON->new->utf8(1)->allow_blessed->convert_blessed->encode(
        {
            report => problem_as_hashref($c, $problem),
            updates => $updates ? $updates : '',
        }
    );
    $c->res->body( $content );
    return 1;

}

sub generate_map_tags : Private {
    my ( $self, $c ) = @_;

    my $problem = $c->stash->{problem};

    $c->stash->{page} = 'report';
    FixMyStreet::Map::display_map(
        $c,
        latitude  => $problem->latitude,
        longitude => $problem->longitude,
        pins      => $problem->used_map
        ? [ {
            latitude  => $problem->latitude,
            longitude => $problem->longitude,
            colour    => $c->cobrand->pin_colour($problem, 'report'),
            type      => 'big',
          } ]
        : [],
    );

    return 1;
}

=head2 around

Find the location search and display nearby reports (for pc or lat,lon).

For x,y searches convert to lat,lon and 301 redirect to them.

If no search redirect back to the homepage.

=cut

sub around_me : Path('around') : Args(0) {
    my ( $self, $c ) = @_;

    # handle old coord systems
    $c->forward('redirect_en_or_xy_to_latlon');

    # Check if we have a partial report
    my $partial_report = $c->forward('load_partial');

    # Try to create a location for whatever we have
    my $ret = $c->forward('/location/determine_location_from_coords')
        || $c->forward('/location/determine_location_from_pc');


    if( $ret && $ret != -1){
	    # Show the nearby reports
	    $c->detach('display_location');

    }else{

    	$c->res->content_type('application/json; charset=utf-8');
	    my $content = JSON->new->utf8(1)->allow_blessed->convert_blessed->encode(
	        {
	            error => 'Invalid coords or address.'
	        }
	    );
	    $c->res->body( $content );

    }

}

=head2 redirect_en_or_xy_to_latlon

    # detaches if there was a redirect
    $c->forward('redirect_en_or_xy_to_latlon');

Handle coord systems that are no longer in use.

=cut

sub redirect_en_or_xy_to_latlon : Private {
    my ( $self, $c ) = @_;
    my $req = $c->req;

    # check for x,y or e,n requests
    my $x = $req->param('x');
    my $y = $req->param('y');
    my $e = $req->param('e');
    my $n = $req->param('n');

    # lat and lon - fill in below if we need to
    my ( $lat, $lon );

    if ( $x || $y ) {
        ( $lat, $lon ) = FixMyStreet::Map::tile_xy_to_wgs84( $x, $y );
        ( $lat, $lon ) = map { Utils::truncate_coordinate($_) } ( $lat, $lon );
    }
    elsif ( $e || $n ) {
        ( $lat, $lon ) = Utils::convert_en_to_latlon_truncated( $e, $n );
    }
    else {
        return;
    }

    # create a uri and redirect to it
    my $ll_uri = $c->uri_for( '/around', { lat => $lat, lon => $lon } );
    $c->res->redirect( $ll_uri, 301 );
    $c->detach;
}

=head2 load_partial

    my $partial_report = $c->forward('load_partial');

Check for the partial token and load the partial report. If found save it and
token to stash and return report. Otherwise return false.

=cut

sub load_partial : Private {
    my ( $self, $c ) = @_;

    my $partial = scalar $c->req->param('partial')
      || return;

    # is it in the database
    my $token =
      $c->model("DB::Token")
      ->find( { scope => 'partial', token => $partial } )    #
      || last;

    # can we get an id from it?
    my $report_id = $token->data                             #
      || last;

    # load the related problem
    my $report = $c->cobrand->problems                       #
      ->search( { id => $report_id, state => 'partial' } )   #
      ->first
      || last;

    # save what we found on the stash.
    $c->stash->{partial_token}  = $token;
    $c->stash->{partial_report} = $report;

    return $report;
}

=head2 display_location

Display a specific lat/lng location (which may have come from a pc search).

=cut

sub display_location : Private {
    my ( $self, $c ) = @_;

    # set the template to use
    $c->stash->{template} = 'around/display_location.html';

    # get the lat,lng
    my $latitude  = $c->stash->{latitude};
    my $longitude = $c->stash->{longitude};

    # truncate the lat,lon for nicer rss urls, and strings for outputting
    my $short_latitude  = Utils::truncate_coordinate($latitude);
    my $short_longitude = Utils::truncate_coordinate($longitude);
    $c->stash->{short_latitude}  = $short_latitude;
    $c->stash->{short_longitude} = $short_longitude;

    # Deal with pin hiding/age
    my $all_pins = $c->req->param('all_pins') ? 1 : undef;
    $c->stash->{all_pins} = $all_pins;
    my $interval = $all_pins ? undef : $c->cobrand->on_map_default_max_pin_age;

    # get the map features
    my ( $on_map_all, $on_map, $around_map, $distance ) =
      FixMyStreet::Map::map_features( $c, $short_latitude, $short_longitude,
        $interval );

    # copy the found reports to the stash
    $c->stash->{on_map}     = $on_map;
    $c->stash->{around_map} = $around_map;
    $c->stash->{distance}   = $distance;

    # create a list of all the pins
    my @pins;
    unless ($c->req->param('no_pins') || $c->cobrand->moniker eq 'emptyhomes') {

        @pins = map {
            # Here we might have a DB::Problem or a DB::Nearby, we always want the problem.
            my $p = (ref $_ eq 'FixMyStreet::App::Model::DB::Nearby') ? $_->problem : $_;
            my $colour = $c->cobrand->pin_colour( $p, 'around' );

            my $updates = $c->model('DB::Comment')->search(
                { problem_id => $p->id, state => 'confirmed' },
                { order_by => { -asc =>'confirmed'} }
            );

            my $updates_json = [];
            while(my $com = $updates->next){

                push @$updates_json,{
                    text    => $com->text,
                    name    => $com->name,
                    created => $com->created->strftime('%H:%M %Y-%m-%d'),
                }
            }

            my $problem_formatted = problem_as_hashref($c, $p);
            $problem_formatted->{comments} = $updates_json;
            $problem_formatted;

        } @$on_map_all, @$around_map;
    }

    $c->stash->{page} = 'around'; # So the map knows to make clickable pins, update on pan
    FixMyStreet::Map::display_map(
        $c,
        latitude  => $short_latitude,
        longitude => $short_longitude,
        clickable => 1,
        pins      => \@pins,
        area      => $c->cobrand->areas_on_around,
    );

	$c->res->content_type('application/json; charset=utf-8');
    my $content = JSON->new->utf8(1)->encode(
        {
          reports_around => \@pins,
	        latitude  => $short_latitude,
	        longitude => $short_longitude,
	        clickable => 1,
	        area      => $c->cobrand->areas_on_around,

        }
    );
    $c->res->body( $content );

    return 1;
}

=head2 report_update

Update a report.

=cut

sub report_update : Path('report/update') : Args(0)  {
    my ( $self, $c ) = @_;

    my $id = $c->req->param('id');
    my $email = $c->req->param('email');
    my $error = '';

    # try to load a report if the id is a number
    my $problem
      = ( !$id || $id =~ m{\D} ) # is id non-numeric?
      ? undef                    # ...don't even search
      : $c->cobrand->problems->find( { id => $id } );

    # check that the problem is suitable to show.
    if ( !$problem || ($problem->state eq 'unconfirmed' && !$c->cobrand->show_unconfirmed_reports) || $problem->state eq 'partial' ) {
        $error = 'Report not found';
    } elsif ( $problem->state eq 'hidden' ) {
        $error = 'Report removed';
    } elsif ( $problem->non_public ) {
        if ( !$c->user || $c->user->id != $problem->user->id ) {
            $error = 'Report not visible';
        }
    }

    # check user
    unless ( $c->forward( '/auth/sign_in', [ $email ] ) ) {
        $error = 'Error with your email/password';
    }

    if($error eq ''){

        my %params =
          map { $_ => scalar $c->req->param($_) } ( 'update', 'name', 'fixed', 'state', 'reopen' );

        $params{update} =
          Utils::cleanup_text( $params{update}, { allow_multiline => 1 } );

        my $name = Utils::trim_text( $params{name} );
        my $anonymous = $c->req->param('may_show_name') ? 0 : 1;

        $params{reopen} = 0 unless $c->user && $c->user->id == $problem->user->id;

        my $update = $c->model('DB::Comment')->new(
            {
                text         => $params{update},
                name         => $name,
                problem      => $problem,
                state        => 'confirmed',
                mark_fixed   => $params{fixed} ? 1 : 0,
                mark_open    => $params{reopen} ? 1 : 0,
                cobrand      => $c->cobrand->moniker,
                cobrand_data => '',
                lang         => 'GB',
                anonymous    => $anonymous,
                user_id      => $c->user->id,
            }
        );

        $update->insert;
        $update->confirm;
        $update->update;

        $c->res->content_type('application/json; charset=utf-8');
        my $content = JSON->new->utf8(1)->encode(
            {
                message => 'updated',
            }
        );
        $c->res->body( $content );

    }else{

        $c->res->content_type('application/json; charset=utf-8');
        my $content = JSON->new->utf8(1)->encode(
            {
                error => $error,
            }
        );
        $c->res->body( $content );
    }

    return 1;

}

sub str_replace : Private {
    my $replace_this = shift;
    my $with_this  = shift;
    my $string   = shift;

    my $length = length($string);
    my $target = length($replace_this);

    for(my $i=0; $i<$length - $target + 1; $i++) {
        if(substr($string,$i,$target) eq $replace_this) {
            $string = substr($string,0,$i) . $with_this . substr($string,$i+$target);
            # return $string; #Comment this if you what a global replace
        }
    }
    return $string;
}



__PACKAGE__->meta->make_immutable;

1;
